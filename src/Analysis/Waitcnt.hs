{-# LANGUAGE TupleSections #-}

module Analysis.Waitcnt (checkWaitcnts) where

import ControlFlow
import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Disassembler
import Reporting
import Text.Regex.TDFA ((=~))

checkWaitcnts :: DisassembledKernel -> CFG -> [LogMessage]
checkWaitcnts _ cfg@(CFG bbs) = Map.foldrWithKey' printMessage [] logMap
  where
    emptyCtx = IterCtx {ctxInCounters = Map.singleton 0 Map.empty, ctxLog = Map.empty}
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

data MemCounter = CounterVMem | CounterSLoad | CounterLDS
  deriving (Eq, Ord, Show)

type GprMemCounters = Map PC (Map MemCounter Int)

type BbMemCounters = Map Gpr GprMemCounters

data WaitcntLocation = WaitcntLocation {locGprUsedAt, locOpIssuedAt :: !PC, locOperand :: !Operand}
  deriving (Eq, Show)

instance Ord WaitcntLocation where
  compare WaitcntLocation {locGprUsedAt = pc, locOpIssuedAt = sourcePc} WaitcntLocation {locGprUsedAt = pc2, locOpIssuedAt = sourcePc2} =
    mconcat [compare pc pc2, compare sourcePc sourcePc2]

type Log = Map WaitcntLocation (Map WaitCounter Int)

data WaitCounter = WaitVmcnt | WaitLgkmcnt
  deriving (Eq, Ord)

data IterCtx = IterCtx {ctxInCounters :: !(Map BasicBlockIdx BbMemCounters), ctxLog :: !Log}

analyzeCfg :: CFG -> IterCtx -> BasicBlockIdx -> IterCtx
analyzeCfg (CFG bbs) ctx bbIdx = foldr analyzeSuccessor ctx {ctxLog = Map.union outLog (ctxLog ctx)} (bbSuccessors currBb)
  where
    currBb = bbs !! bbIdx
    Just inCounters = Map.lookup bbIdx (ctxInCounters ctx)
    (outCounters, outLog) = analyzeInstructions (bbInstructions currBb) (inCounters, ctxLog ctx)
    analyzeSuccessor succIdx succCtx = case Map.lookup succIdx (ctxInCounters succCtx) of
      Just succCounters | outCounters `Map.isSubmapOf` succCounters -> succCtx
      _ -> analyzeCfg (CFG bbs) succCtx {ctxInCounters = Map.insertWith Map.union succIdx outCounters (ctxInCounters ctx)} succIdx
    analyzeInstructions [] (bbCounters, log) = (bbCounters, log)
    analyzeInstructions ((pc, i) : next) (bbCounters, log) =
      case i of
        Instruction "s_waitcnt" [OConst 0] ->
          analyzeInstructions next (Map.empty, log)
        Instruction "s_waitcnt" expr ->
          analyzeInstructions next (updateCountersOnWaitcnt expr bbCounters, log)
        Instruction _ _ ->
          --trace ("dst: " ++ show dstGprs ++ ", srcs: " ++ show srcGprs ++ ", ctrs: " ++ show bbCounters') $
          analyzeInstructions next (bbCounters'', log')
          where
            (dstGprs, srcGprs) = extractDstSrcGprs i
            counters = extractGprCounters i
            (bbCounters', log') = foldl' (checkSrcPendingLoads pc) (bbCounters, log) srcGprs
            bbCounters'' = case counters of
              Just (ctrGprs, opCtrs) -> addCounters pc opCtrs ctrGprs $ incCountersOnMemoryOp pc opCtrs bbCounters'
              _ -> foldr Map.delete bbCounters' dstGprs -- drop overwritten gprs

addCounters :: PC -> [MemCounter] -> [Gpr] -> BbMemCounters -> BbMemCounters
addCounters pc ctrs gprs bbCounters = foldr insertGprCounter bbCounters gprs
  where
    insertGprCounter gpr = Map.insertWith (Map.unionWith min) gpr (Map.singleton pc $ Map.fromList $ (,0) <$> ctrs)

checkSrcPendingLoads :: PC -> (BbMemCounters, Log) -> Gpr -> (BbMemCounters, Log)
checkSrcPendingLoads pc (bbCtrs, log) gpr = case Map.lookup gpr bbCtrs of
  Just gprCtrs -> (Map.delete gpr bbCtrs, log')
    where
      log' = Map.foldrWithKey' updateLog log gprCtrs
      operand = case gpr of Sgpr s -> Osgpr [s]; Vgpr v -> Ovgpr [v]
      updateLog ctrOriginPc ctrs = Map.insert location waits
        where
          location = WaitcntLocation {locGprUsedAt = pc, locOpIssuedAt = ctrOriginPc, locOperand = operand}
          existingWaits = fromMaybe Map.empty $ Map.lookup location log
          waits = Map.foldrWithKey waitForCtr existingWaits ctrs
          waitForCtr CounterVMem val = Map.insertWith min WaitVmcnt val
          -- If there are multiple types of lgkmcnt instructions in flight, they will return out of order so we need to wait for all of them.
          waitForCtr CounterLDS val
            | CounterSLoad `Set.member` outstandingCtrs = Map.insert WaitLgkmcnt 0
            | otherwise = Map.insert WaitLgkmcnt val
          -- Scalar loads are always returned out of order so we need to wait for lgkmcnt to reach 0.
          waitForCtr CounterSLoad _ = Map.insert WaitLgkmcnt 0
      outstandingCtrs = Map.foldl' (Map.foldrWithKey (\ctrPc ctrMap -> Set.union (if ctrPc < pc then Map.keysSet ctrMap else Set.empty))) Set.empty bbCtrs
  _ -> (bbCtrs, log)

updateCountersOnWaitcnt :: [Operand] -> BbMemCounters -> BbMemCounters
updateCountersOnWaitcnt waitops = Map.mapMaybe dropCounters
  where
    dropCounters ctrsByPc = if Map.null newCtrs then Nothing else Just newCtrs
      where
        newCtrs = Map.mapMaybe countersAfterWait ctrsByPc
        countersAfterWait :: Map MemCounter Int -> Maybe (Map MemCounter Int)
        countersAfterWait ctrs = if Map.null ctrs' then Nothing else Just ctrs'
          where
            ctrs' = Map.filterWithKey (\ty c -> not (waitedFor ty c)) ctrs
            waitedFor ty c = case find ((== ty) . fst) countersWaitedFor of
              Just (_, cWaited) | c >= cWaited -> True
              _ -> False
    countersWaitedFor = parseCounter =<< waitops
    parseCounter :: Operand -> [(MemCounter, Int)]
    parseCounter (OOther expr)
      | [[_, c]] <- expr =~ "vmcnt\\(([0-9]+)\\)" = [(CounterVMem, read c)]
      | otherwise = []

incCountersOnMemoryOp :: PC -> [MemCounter] -> BbMemCounters -> BbMemCounters
incCountersOnMemoryOp pc opCtrs = Map.map $ Map.map $ Map.mapWithKey (\ty c -> if ty `elem` opCtrs then c + 1 else c)

extractDstSrcGprs :: Instruction -> ([Gpr], [Gpr])
extractDstSrcGprs i = case i of
  Instruction opcode operands | opcode =~ "^buffer_store" -> ([], extractGprs operands)
  Instruction _ (dst : srcs) -> (extractGprs [dst], extractGprs srcs)
  _ -> ([], [])
  where
    extractGprs = foldr extractGpr []
    extractGpr (Osgpr sgprIdxs) gprs = (Sgpr <$> sgprIdxs) ++ gprs
    extractGpr (Ovgpr vgprIdxs) gprs = (Vgpr <$> vgprIdxs) ++ gprs
    extractGpr _ gprs = gprs

extractGprCounters :: Instruction -> Maybe ([Gpr], [MemCounter])
extractGprCounters (Instruction opcode (Ovgpr vdst : _))
  | opcode =~ "^buffer_(load|atomic)" = Just (Vgpr <$> vdst, [CounterVMem])
  | opcode =~ "^buffer_store" = Just ([], [CounterVMem])
  | opcode =~ "^ds_read" = Just (Vgpr <$> vdst, [CounterLDS])
  | opcode =~ "^ds_write" = Just ([], [CounterLDS])
  | otherwise = Nothing
extractGprCounters (Instruction opcode (Osgpr sdst : _))
  | opcode =~ "^s_(buffer_)?load" = Just (Sgpr <$> sdst, [CounterSLoad])
  | otherwise = Nothing
extractGprCounters _ = Nothing
