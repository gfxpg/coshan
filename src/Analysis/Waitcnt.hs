{-# LANGUAGE TupleSections #-}

module Analysis.Waitcnt (checkWaitcnts) where

import ControlFlow
import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
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
        waitcntOps ctrs = unwords (waitcntOp <$> Map.toList ctrs)
        waitcntOp (CounterVLoad, c) = "vmcnt(" ++ show c ++ ")"
        waitcntOp (CounterVStore, c) = "vmcnt(" ++ show c ++ ")" -- TODO: vscnt on GFX10

data Gpr = Sgpr Int | Vgpr Int
  deriving (Eq, Ord, Show)

data MemCounter = CounterVLoad | CounterVStore
  deriving (Eq, Ord, Show)

type GprMemCounters = Map PC (Map MemCounter Int)

type BbMemCounters = Map Gpr GprMemCounters

data WaitcntLocation = WaitcntLocation {locGprUsedAt, locOpIssuedAt :: !PC, locOperand :: !Operand}
  deriving (Eq, Show)

instance Ord WaitcntLocation where
  compare WaitcntLocation {locGprUsedAt = pc, locOpIssuedAt = sourcePc} WaitcntLocation {locGprUsedAt = pc2, locOpIssuedAt = sourcePc2} =
    mconcat [compare pc pc2, compare sourcePc sourcePc2]

type Log = Map WaitcntLocation (Map MemCounter Int)

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
        Instruction _ _
          | Just (dstRegs, opCtrs) <- extractMemoryDstAndCounters i,
            bbCounters' <- incCountersOnMemoryOp pc opCtrs bbCounters,
            bbCounters'' <- addCounters bbCounters' pc opCtrs dstRegs ->
            analyzeInstructions next (bbCounters'', log)
        Instruction "s_waitcnt" [OConst 0] ->
          analyzeInstructions next (Map.empty, log)
        Instruction "s_waitcnt" expr ->
          analyzeInstructions next (updateCountersOnWaitcnt expr bbCounters, log)
        Instruction _ (dst : srcs)
          | (bbCounters', log') <- foldl' (checkSrcPendingLoads pc) (bbCounters, log) srcs,
            bbCounters'' <- dropOverwrittenGprs dst bbCounters' ->
            analyzeInstructions next (bbCounters'', log')
        _ ->
          analyzeInstructions next (bbCounters, log)

addCounters :: BbMemCounters -> PC -> [MemCounter] -> [Gpr] -> BbMemCounters
addCounters bbCounters pc ctrs = foldr insertGprCounter bbCounters
  where
    insertGprCounter gpr = Map.insertWith (Map.unionWith min) gpr (Map.singleton pc $ Map.fromList $ (,0) <$> ctrs)

dropOverwrittenGprs :: Operand -> BbMemCounters -> BbMemCounters
dropOverwrittenGprs (Osgpr sgprs) bbCtrs = foldr (Map.delete . Sgpr) bbCtrs sgprs
dropOverwrittenGprs (Ovgpr vgprs) bbCtrs = foldr (Map.delete . Vgpr) bbCtrs vgprs
dropOverwrittenGprs _ bbCtrs = bbCtrs

checkSrcPendingLoads :: PC -> (BbMemCounters, Log) -> Operand -> (BbMemCounters, Log)
checkSrcPendingLoads pc (bbCtrs, log) operand =
  case operand of
    Osgpr (sgprIdx : sgprRest) -> checkSrcPendingLoads pc (checkRegister (Osgpr [sgprIdx]) (Sgpr sgprIdx)) (Osgpr sgprRest)
    Ovgpr (vgprIdx : vgprRest) -> checkSrcPendingLoads pc (checkRegister (Ovgpr [vgprIdx]) (Vgpr vgprIdx)) (Ovgpr vgprRest)
    _ -> (bbCtrs, log)
  where
    checkRegister gprOp gpr = case Map.lookup gpr bbCtrs of
      Just gprCtrs -> (Map.delete gpr bbCtrs, log')
        where
          log' = Map.foldrWithKey' updateLog log gprCtrs
          updateLog ctrOriginPc ctrs log =
            let location = WaitcntLocation {locGprUsedAt = pc, locOpIssuedAt = ctrOriginPc, locOperand = operand}
             in Map.insertWith (Map.unionWith min) location ctrs log
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
      | [[_, c]] <- expr =~ "vmcnt\\(([0-9]+)\\)" = [(CounterVLoad, read c)]
      | otherwise = []

incCountersOnMemoryOp :: PC -> [MemCounter] -> BbMemCounters -> BbMemCounters
incCountersOnMemoryOp pc opCtrs = Map.map $ Map.map $ Map.mapWithKey (\ty c -> if ty `elem` opCtrs then c + 1 else c)

extractMemoryDstAndCounters :: Instruction -> Maybe ([Gpr], [MemCounter])
extractMemoryDstAndCounters (Instruction opcode (Ovgpr vdst : _))
  | opcode =~ "buffer_(load|atomic)_" = Just (Vgpr <$> vdst, [CounterVLoad])
  | opcode =~ "buffer_store_" = Just (Vgpr <$> vdst, [CounterVStore])
  | otherwise = Nothing
extractMemoryDstAndCounters _ = Nothing
