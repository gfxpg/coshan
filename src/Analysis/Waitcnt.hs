module Analysis.Waitcnt (checkWaitcnts) where

import ControlFlow
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Disassembler
import Text.Regex.TDFA ((=~))

checkWaitcnts :: DisassembledKernel -> CFG -> [String]
checkWaitcnts _ cfg@(CFG (startBb : _)) =
  let emptyCtx = IterCtx {ctxRegCounters = Map.empty, ctxWalkedIdx = [], ctxSuccIdxs = [], ctxLog = []}
   in ctxLog $ analyzeBb cfg startBb emptyCtx

data Gpr = Sgpr Int | Vgpr Int
  deriving (Eq, Ord)

data MemCounters = MemCounters {ctrVMemLoad, ctrVMemStore :: Maybe Int}

memCounters :: MemCounters
memCounters = MemCounters Nothing Nothing

data IterCtx = IterCtx
  { ctxRegCounters :: Map Gpr MemCounters,
    ctxWalkedIdx, ctxSuccIdxs :: [BasicBlockIdx],
    ctxLog :: [String]
  }

-- TODO: follow successors
analyzeBb :: CFG -> BasicBlock -> IterCtx -> IterCtx
analyzeBb (CFG bbs) currBb ctx = analyzeInstructions (bbInstructions currBb) ctx
  where
    analyzeInstructions [] ctx = ctx
    analyzeInstructions ((pc, i) : next) ctx =
      case i of
        Instruction _ _
          | Just (dstRegs, opCtrs) <- extractMemoryDstAndCounters i,
            counters' <- updateCountersOnNewMemoryOp opCtrs (ctxRegCounters ctx),
            ctx' <- ctx {ctxRegCounters = foldl' (\m k -> Map.insert k opCtrs m) counters' dstRegs} ->
            analyzeInstructions next ctx'
        Instruction _ (_dst : srcs) ->
          analyzeInstructions next $ foldl' checkSrcPendingLoads ctx srcs
        _ ->
          analyzeInstructions next ctx

checkSrcPendingLoads :: IterCtx -> Operand -> IterCtx
checkSrcPendingLoads ctx operand =
  case operand of
    Osgpr (sgprIdx : sgprRest) -> checkSrcPendingLoads (checkRegister (Sgpr sgprIdx)) (Osgpr sgprRest)
    Ovgpr (vgprIdx : vgprRest) -> checkSrcPendingLoads (checkRegister (Vgpr vgprIdx)) (Ovgpr vgprRest)
    _ -> ctx
  where
    checkRegister gpr
      | Just ctrs <- Map.lookup gpr (ctxRegCounters ctx) =
        let missingWaitcnt ctrs
              | Just c <- ctrVMemLoad ctrs = "vmcnt(" ++ show c ++ ")"
              | Just c <- ctrVMemStore ctrs = "vmcnt(" ++ show c ++ ")" -- TODO: vscnt on GFX10
         in ctx {ctxRegCounters = Map.delete gpr (ctxRegCounters ctx), ctxLog = missingWaitcnt ctrs : ctxLog ctx}
      | otherwise = ctx

updateCountersOnNewMemoryOp :: MemCounters -> Map Gpr MemCounters -> Map Gpr MemCounters
updateCountersOnNewMemoryOp opCtrs = Map.map $ \ctrs ->
  let inc getCounter = (+ (if isJust (getCounter opCtrs) then 1 else 0)) <$> getCounter ctrs
   in MemCounters {ctrVMemLoad = inc ctrVMemLoad, ctrVMemStore = inc ctrVMemStore}

extractMemoryDstAndCounters :: Instruction -> Maybe ([Gpr], MemCounters)
extractMemoryDstAndCounters (Instruction opcode (Ovgpr vdst : _))
  | opcode =~ "buffer_(load|atomic)_" = Just (Vgpr <$> vdst, memCounters {ctrVMemLoad = Just 0})
  | opcode =~ "buffer_store_" = Just (Vgpr <$> vdst, memCounters {ctrVMemStore = Just 0})
  | otherwise = Nothing
extractMemoryDstAndCounters _ = Nothing
