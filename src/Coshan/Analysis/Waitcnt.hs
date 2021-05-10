module Coshan.Analysis.Waitcnt (checkWaitcnts) where

import Coshan.ControlFlow
import Coshan.Disassembler
import qualified Coshan.Reporting as R
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BC8
import Data.List (foldl', intercalate, intersect, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

checkWaitcnts :: DisassembledKernel -> CFG -> [R.Error]
checkWaitcnts _ cfg = Map.foldrWithKey' printMessage [] logMap
  where
    emptyCtx = IterCtx {ctxVisitedOutEvents = Map.empty, ctxLog = Map.empty}
    logMap = ctxLog $ foldlWithSuccessors' analyzeBb emptyCtx [] 0 cfg
    printMessage loc (waitctr, explanation) log =
      let traceEvent (i, (pc, [], etype)) = (pc, "(" ++ show i ++ ")" ++ " " ++ show etype)
          traceEvent (i, (pc, gprs, etype)) = (pc, "(" ++ show i ++ ")" ++ " " ++ show etype ++ ": " ++ intercalate ", " (show <$> gprs))
          explRegs = case locGprs loc of
            [r] -> "register " ++ show r ++ " is"
            rs -> "registers " ++ intercalate ", " (show <$> rs) ++ " are"
          violation =
            R.CounterWaitRequired
              { R.ctrreqWaitcntClause = waitctr,
                R.ctrreqSucceedingEvents = traceEvent <$> zip [(0 :: Int) ..] (locQueueSucc loc),
                R.ctrreqPrecedingEvents = traceEvent <$> zip [(length $ locQueueSucc loc) ..] (locQueuePred loc),
                R.ctrreqExplanation = "Source " ++ explRegs ++ " read from memory. " ++ explanation
              }
       in R.Error (locGprUsedAt loc) violation : log

data Gpr = Sgpr Int | Vgpr Int
  deriving (Eq, Ord)

instance Show Gpr where
  show (Sgpr i) = "s" ++ show i
  show (Vgpr i) = "v" ++ show i

data MemEvent = EventVMem | EventSLoad | EventLDS
  deriving (Eq, Ord)

instance Show MemEvent where
  show EventVMem = "vmem access"
  show EventSLoad = "scalar read (returned out-of-order)"
  show EventLDS = "lds access"

data WaitcntLocation = WaitcntLocation {locGprUsedAt :: !PC, locGprs :: [Gpr], locQueueSucc :: [(PC, [Gpr], MemEvent)], locQueuePred :: [(PC, [Gpr], MemEvent)]}
  deriving (Eq, Show)

instance Ord WaitcntLocation where
  compare WaitcntLocation {locGprUsedAt = pc, locGprs = gprs} WaitcntLocation {locGprUsedAt = pc2, locGprs = gprs2} =
    mconcat [compare pc pc2, compare gprs gprs2]

type Log = Map WaitcntLocation (Operand, String)

data IterCtx = IterCtx {ctxVisitedOutEvents :: !(Map BasicBlockIdx (Set PendingEvents)), ctxLog :: !Log}

type PendingEvents = [(PC, [Gpr], MemEvent)]

analyzeBb :: IterCtx -> PendingEvents -> (BasicBlock, BasicBlockIdx) -> (IterCtx, Maybe PendingEvents)
analyzeBb ctx inEvents (BasicBlock {bbInstructions = instructions}, bbIdx) =
  case Map.lookup bbIdx (ctxVisitedOutEvents ctx) of
    Just visitedOutEvents | nub outEvents `elem` visitedOutEvents -> (ctx', Nothing)
    _ -> (ctx', Just outEvents)
  where
    ctx' = ctx {ctxLog = Map.union outLog (ctxLog ctx), ctxVisitedOutEvents = Map.insertWith Set.union bbIdx (Set.singleton (nub outEvents)) (ctxVisitedOutEvents ctx)}
    (outEvents, outLog) = analyzeInstructions instructions (inEvents, ctxLog ctx)
    analyzeInstructions [] (bbEvents, log) = (bbEvents, log)
    analyzeInstructions ((pc, i) : next) (bbEvents, log) =
      case i of
        Instruction ["s", "waitcnt"] expr ->
          analyzeInstructions next (updateEventsOnWaitcnt expr bbEvents, log)
        Instruction _ _ ->
          analyzeInstructions next (bbEvents'', log')
          where
            (_dstGprs, srcGprs) = extractDstSrcGprs i
            (bbEvents', log') = checkPendingEvents pc (bbEvents, log) srcGprs
            bbEvents'' = case extractGprEvents i of
              Just (eventGprs, events) -> addEvents pc events eventGprs bbEvents'
              _ -> bbEvents' -- todo drop overwritten gprs?

addEvents :: PC -> [MemEvent] -> [Gpr] -> PendingEvents -> PendingEvents
addEvents pc etypes gprs pevents = ((pc,gprs,) <$> etypes) ++ pevents

checkPendingEvents :: PC -> (PendingEvents, Log) -> [Gpr] -> (PendingEvents, Log)
-- TODO issue one warning per PC, group pending events into one s_waitcnt expression
checkPendingEvents checkPc (pevents, log) checkGprs =
  {-trace ("CHK PC" ++ show checkPc ++ show checkGprs ++ ", pevents: " ++ show pevents ++ ", out: " ++ show (pevents', log')) $-} (pevents', log')
  where
    pendingEvents = filter (\(_, gprs, _) -> not $ null $ checkGprs `intersect` gprs) pevents
    (missingWaits, log') = foldl' logGprUsageWithPendingEvent ([], log) pendingEvents
    pevents' = updateEventsOnWaitcnt missingWaits pevents
    logGprUsageWithPendingEvent (waitcnt, log) (epc, egprs, etype) = (counterForEvent : waitcnt, Map.insert location (counterForEvent, explanation) log)
      where
        pendingGprs = checkGprs `intersect` egprs
        location = WaitcntLocation {locGprs = pendingGprs, locGprUsedAt = checkPc, locQueueSucc = succEvents, locQueuePred = currAndPrecedingEvents}
        explanationAwaitSuccs 1 = "The operation is complete when the counter reaches 1 because there is one operation enqueued after it."
        explanationAwaitSuccs c = "The operation is complete when the counter reaches " ++ show c ++ " because there are " ++ show c ++ " operations enqueued after it."
        explanationOutOfOrder = "The operation is complete only when the counter reaches 0 because there are out-of-order operations enqueued."
        (counterForEvent, explanation) = case (etype, counterReturnsOutOfOrder) of
          (EventVMem, _) -> let c = length succEvents in (Ovmcnt c, explanationAwaitSuccs c)
          (_, False) -> let c = length succEvents in (Olgkmcnt c, explanationAwaitSuccs c)
          (_, True) -> (Olgkmcnt 0, explanationOutOfOrder)
        ((succEvents, currAndPrecedingEvents), counterReturnsOutOfOrder) = case etype of
          EventVMem ->
            let ofType = filter (\(_, _, ty) -> ty == EventVMem) pevents
             in (break (\(pc, _, _) -> pc == epc) ofType, False)
          _ ->
            break (\(pc, _, _) -> pc == epc)
              `first` foldr
                ( \e@(_, _, ty) (es, mixed) ->
                    if ty `elem` [EventLDS, EventSLoad] then (e : es, mixed || ty == EventSLoad) else (es, mixed)
                )
                ([], False)
                pevents

updateEventsOnWaitcnt :: [Operand] -> PendingEvents -> PendingEvents
updateEventsOnWaitcnt waitops pevents = foldl' updateOutstanding pevents (opToEvents =<< waitops)
  where
    updateOutstanding :: [(PC, [Gpr], MemEvent)] -> (MemEvent, Int) -> [(PC, [Gpr], MemEvent)]
    updateOutstanding q (eType, eTargetCount) = fst $ foldr dropEvent ([], countToDrop) q
      where
        countToDrop = max 0 (countOfType - eTargetCount)
        countOfType = length $ filter (\(_, _, ty) -> ty == eType) q
        dropEvent e (q, 0) = (e : q, 0)
        dropEvent e@(_, _, ty) (q, toDrop)
          | ty == eType = (q, toDrop - 1)
          | otherwise = (e : q, toDrop)
    opToEvents (Ovmcnt c) = [(EventVMem, c)]
    opToEvents (Olgkmcnt c) = [(EventSLoad, c), (EventLDS, c)]
    opToEvents (Oexpcnt _) = []
    opToEvents o = error $ "Unexpected s_waitcnt operand " ++ show o

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
