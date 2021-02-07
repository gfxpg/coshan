{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Coshan.Analysis.Waitcnt (checkWaitcnts) where

import Coshan.ControlFlow
import Coshan.Disassembler
import Coshan.Reporting
import qualified Data.ByteString.Char8 as BC8
import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Debug.Trace

checkWaitcnts :: DisassembledKernel -> CFG -> [LogMessage]
checkWaitcnts _ cfg = Map.foldrWithKey' printMessage [] logMap
  where
    emptyCtx = IterCtx {ctxInEvents = Map.singleton 0 Map.empty, ctxLog = Map.empty}
    logMap = ctxLog $ analyzeCfg cfg emptyCtx 0
    printMessage loc ctrs log =
      LogMessage
        (locGprUsedAt loc)
        [ LogText "Missing",
          LogInstruction ("s_waitcnt " ++ waitcntOps ctrs),
          LogText "before accessing register",
          LogOperand (locOperand loc),
          LogText "read from memory at",
          LogInstructionPath [locOpIssuedAt loc]
        ] :
      log
      where
        waitcntOps ctrs = unwords (showCtr <$> Map.toList ctrs)
        showCtr (WaitVmcnt, c) = "vmcnt(" ++ show c ++ ")"
        showCtr (WaitLgkmcnt, c) = "lgkmcnt(" ++ show c ++ ")"

data Gpr = Sgpr Int | Vgpr Int
  deriving (Eq, Ord, Show)

data MemEvent = EventVMem | EventSLoad | EventLDS
  deriving (Eq, Ord, Show)

type GprMemEvents = Map PC (Map MemEvent Int)

type BbMemEvents = Map Gpr GprMemEvents

data WaitcntLocation = WaitcntLocation {locGprUsedAt, locOpIssuedAt :: !PC, locOperand :: !Operand}
  deriving (Eq, Show)

instance Ord WaitcntLocation where
  compare WaitcntLocation {locGprUsedAt = pc, locOpIssuedAt = sourcePc} WaitcntLocation {locGprUsedAt = pc2, locOpIssuedAt = sourcePc2} =
    mconcat [compare pc pc2, compare sourcePc sourcePc2]

type Log = Map WaitcntLocation (Map WaitCounter Int)

data WaitCounter = WaitVmcnt | WaitLgkmcnt
  deriving (Eq, Ord)

data IterCtx = IterCtx {ctxInEvents :: !(Map BasicBlockIdx BbMemEvents), ctxLog :: !Log}

analyzeCfg :: CFG -> IterCtx -> BasicBlockIdx -> IterCtx
analyzeCfg (CFG bbs) ctx bbIdx = foldr analyzeSuccessor ctx {ctxLog = Map.union outLog (ctxLog ctx)} (bbSuccessors currBb)
  where
    currBb = bbs !! bbIdx
    Just inEvents = Map.lookup bbIdx (ctxInEvents ctx)
    (outEvents, outLog) = analyzeInstructions (bbInstructions currBb) (inEvents, ctxLog ctx)
    analyzeSuccessor succIdx succCtx = case Map.lookup succIdx (ctxInEvents succCtx) of
      Just succEvents | outEvents `Map.isSubmapOf` succEvents -> succCtx
      _ -> analyzeCfg (CFG bbs) succCtx {ctxInEvents = Map.insertWith Map.union succIdx outEvents (ctxInEvents ctx)} succIdx
    analyzeInstructions [] (bbEvents, log) = (bbEvents, log)
    analyzeInstructions ((pc, i) : next) (bbEvents, log) =
      case i of
        Instruction ["s", "waitcnt"] expr ->
          analyzeInstructions next (updateEventsOnWaitcnt expr bbEvents, log)
        Instruction _ _ ->
          --trace ("dst: " ++ show dstGprs ++ ", srcs: " ++ show srcGprs ++ ", events: " ++ show bbEvents') $
          analyzeInstructions next (bbEvents'', log')
          where
            (dstGprs, srcGprs) = extractDstSrcGprs i
            (bbEvents', log') = foldl' (checkSrcPendingLoads pc) (bbEvents, log) srcGprs
            bbEvents'' = case extractGprEvents i of
              Just (eventGprs, events) -> addEvents pc events eventGprs $ incExistingEvents pc events bbEvents'
              _ -> foldr Map.delete bbEvents' dstGprs -- drop overwritten gprs

addEvents :: PC -> [MemEvent] -> [Gpr] -> BbMemEvents -> BbMemEvents
addEvents pc events gprs bbEvents = foldr insertGprEvent bbEvents gprs
  where
    insertGprEvent gpr = Map.insertWith (Map.unionWith min) gpr (Map.singleton pc $ Map.fromList $ (,0) <$> events)

checkSrcPendingLoads :: PC -> (BbMemEvents, Log) -> Gpr -> (BbMemEvents, Log)
checkSrcPendingLoads pc (bbEvents, log) gpr = case Map.lookup gpr bbEvents of
  Just gprEvents -> (Map.delete gpr bbEvents, log')
    where
      log' = Map.foldrWithKey' logCountersForEventsIssuedAt log gprEvents
      operand = case gpr of Sgpr s -> Osgpr [s]; Vgpr v -> Ovgpr [v]
      logCountersForEventsIssuedAt issuedAtPc events = Map.insert location counters
        where
          location = WaitcntLocation {locGprUsedAt = pc, locOpIssuedAt = issuedAtPc, locOperand = operand}
          existingWaits = fromMaybe Map.empty $ Map.lookup location log
          counters = Map.foldrWithKey' counterForEvent existingWaits events
          counterForEvent EventVMem val = Map.insertWith min WaitVmcnt val
          -- If there are multiple types of lgkmcnt instructions in flight, they will return out of order so we need to wait for all of them.
          counterForEvent EventLDS val
            | EventSLoad `Set.member` outstandingEventsAtCurrentPc = Map.insert WaitLgkmcnt 0
            | otherwise = Map.insert WaitLgkmcnt val
          -- Scalar loads are always returned out of order so we need to wait for lgkmcnt to reach 0.
          counterForEvent EventSLoad _ = Map.insert WaitLgkmcnt 0
      outstandingEventsAtCurrentPc = Map.foldl' (Map.foldrWithKey' addEventsBeforeCurrentPc) Set.empty bbEvents
      addEventsBeforeCurrentPc eventsPc eventsMap acc
        | eventsPc < pc = acc `Set.union` Map.keysSet eventsMap
        | otherwise = acc
  _ -> (bbEvents, log)

updateEventsOnWaitcnt :: [Operand] -> BbMemEvents -> BbMemEvents
updateEventsOnWaitcnt waitops = Map.mapMaybe dropEvents
  where
    dropEvents eventsByPc = if Map.null newEvents then Nothing else Just newEvents
      where
        newEvents = Map.mapMaybe eventsAfterWait eventsByPc
        eventsAfterWait :: Map MemEvent Int -> Maybe (Map MemEvent Int)
        eventsAfterWait events = if Map.null events' then Nothing else Just events'
          where
            events' = Map.filterWithKey (\ty c -> not (waitedFor ty c)) events
            waitedFor ty c = case find ((== ty) . fst) eventsWaitedFor of
              Just (_, cWaited) | c >= cWaited -> True
              _ -> False
    eventsWaitedFor = opToEvents =<< waitops
    opToEvents (Ovmcnt c) = [(EventVMem, c)]
    opToEvents (Olgkmcnt c) = [(EventSLoad, c), (EventLDS, c)]
    opToEvents (Oexpcnt _) = []
    opToEvents o = error $ "Unexpected s_waitcnt operand " ++ show o

incExistingEvents :: PC -> [MemEvent] -> BbMemEvents -> BbMemEvents
incExistingEvents pc events = Map.map $ Map.map $ Map.mapWithKey (\ty c -> if ty `elem` events then c + 1 else c)

extractDstSrcGprs :: Instruction -> ([Gpr], [Gpr])
extractDstSrcGprs i = case i of
  Instruction ("buffer" : "store" : _) operands -> ([], extractGprs operands)
  Instruction _ (dst : srcs) -> (extractGprs [dst], extractGprs srcs)
  _ -> ([], [])
  where
    extractGprs = foldr extractGpr []
    extractGpr (Osgpr sgprIdxs) gprs = (Sgpr <$> sgprIdxs) ++ gprs
    extractGpr (Ovgpr vgprIdxs) gprs = (Vgpr <$> vgprIdxs) ++ gprs
    extractGpr _ gprs = gprs

extractGprEvents :: Instruction -> Maybe ([Gpr], [MemEvent])
extractGprEvents i = case i of
  Instruction ("buffer" : "load" : _) (Ovgpr vdst : _) -> Just (Vgpr <$> vdst, [EventVMem])
  Instruction ("buffer" : "atomic" : _) (Ovgpr vdst : _) -> Just (Vgpr <$> vdst, [EventVMem])
  Instruction ("buffer" : "store" : _) _ -> Just ([], [EventVMem])
  Instruction ("ds" : dsOp : _) (Ovgpr vdst : _)
    | "read" `BC8.isPrefixOf` dsOp -> Just (Vgpr <$> vdst, [EventLDS])
  Instruction ("ds" : dsOp : _) _
    | "write" `BC8.isPrefixOf` dsOp -> Just ([], [EventLDS])
  Instruction ("s" : "buffer" : "load" : _) (Osgpr sdst : _) -> Just (Sgpr <$> sdst, [EventSLoad])
  Instruction ("s" : "load" : _) (Osgpr sdst : _) -> Just (Sgpr <$> sdst, [EventSLoad])
  _ -> Nothing
