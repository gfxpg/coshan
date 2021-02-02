module Analysis.Waitcnt (checkWaitcnts) where

import ControlFlow
import Data.List (foldl')
import Data.List.Split (dropBlanks, oneOf, split)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Disassembler
import Reporting
import Text.Regex.TDFA ((=~))

checkWaitcnts :: DisassembledKernel -> CFG -> [LogMessage]
checkWaitcnts _ cfg@(CFG (startBb : _)) =
  let emptyCtx = IterCtx {ctxRegCounters = Map.empty, ctxWalkedIdx = [], ctxSuccIdxs = [], ctxLog = []}
   in ctxLog $ analyzeBb cfg startBb emptyCtx

data Gpr = Sgpr Int | Vgpr Int
  deriving (Eq, Ord)

data MemCounters = MemCounters {ctrVMemLoad, ctrVMemStore :: Maybe Int, ctrIssuedAt :: PC}

memCounters :: PC -> MemCounters
memCounters = MemCounters Nothing Nothing

areCountersEmpty :: MemCounters -> Bool
areCountersEmpty (MemCounters Nothing Nothing _) = True
areCountersEmpty _ = False

data IterCtx = IterCtx
  { ctxRegCounters :: Map Gpr MemCounters,
    ctxWalkedIdx, ctxSuccIdxs :: [BasicBlockIdx],
    ctxLog :: [LogMessage]
  }

-- TODO: follow successors
analyzeBb :: CFG -> BasicBlock -> IterCtx -> IterCtx
analyzeBb (CFG bbs) currBb ctx = analyzeInstructions (bbInstructions currBb) ctx
  where
    analyzeInstructions [] ctx = ctx
    analyzeInstructions ((pc, i) : next) ctx =
      case i of
        Instruction _ _
          | Just (dstRegs, opCtrs) <- extractMemoryDstAndCounters (pc, i),
            counters' <- updateCountersOnNewMemoryOp opCtrs (ctxRegCounters ctx),
            ctx' <- ctx {ctxRegCounters = foldl' (\m k -> Map.insert k opCtrs m) counters' dstRegs} ->
            analyzeInstructions next ctx'
        Instruction "s_waitcnt" [OConst 0] ->
          analyzeInstructions next $ ctx {ctxRegCounters = Map.empty}
        Instruction "s_waitcnt" [OOther expr] ->
          analyzeInstructions next $ ctx {ctxRegCounters = updateCountersOnWaitcnt expr (ctxRegCounters ctx)}
        Instruction _ (dst : srcs) ->
          analyzeInstructions next $ dropOverwrittenGprs dst $ foldl' (checkSrcPendingLoads pc) ctx srcs
        _ ->
          analyzeInstructions next ctx

dropOverwrittenGprs :: Operand -> IterCtx -> IterCtx
dropOverwrittenGprs (Osgpr sgprs) ctx = ctx {ctxRegCounters = foldr (Map.delete . Sgpr) (ctxRegCounters ctx) sgprs}
dropOverwrittenGprs (Ovgpr vgprs) ctx = ctx {ctxRegCounters = foldr (Map.delete . Vgpr) (ctxRegCounters ctx) vgprs}
dropOverwrittenGprs _ ctx = ctx

checkSrcPendingLoads :: PC -> IterCtx -> Operand -> IterCtx
checkSrcPendingLoads pc ctx operand =
  case operand of
    Osgpr (sgprIdx : sgprRest) -> checkSrcPendingLoads pc (checkRegister (Osgpr [sgprIdx]) (Sgpr sgprIdx)) (Osgpr sgprRest)
    Ovgpr (vgprIdx : vgprRest) -> checkSrcPendingLoads pc (checkRegister (Ovgpr [vgprIdx]) (Vgpr vgprIdx)) (Ovgpr vgprRest)
    _ -> ctx
  where
    checkRegister gprOp gpr
      | Just ctrs <- Map.lookup gpr (ctxRegCounters ctx) =
        let waitcntOps ctrs
              | Just c <- ctrVMemLoad ctrs = "vmcnt(" ++ show c ++ ")"
              | Just c <- ctrVMemStore ctrs = "vmcnt(" ++ show c ++ ")" -- TODO: vscnt on GFX10
            message =
              LogMessage
                pc
                [ LogText "Missing",
                  LogInstruction ("s_waitcnt " ++ waitcntOps ctrs),
                  LogText "before accessing register",
                  LogOperand gprOp,
                  LogText "read from memory at",
                  LogInstructionPath [ctrIssuedAt ctrs]
                ]
         in ctx {ctxRegCounters = Map.delete gpr (ctxRegCounters ctx), ctxLog = message : ctxLog ctx}
      | otherwise = ctx

updateCountersOnWaitcnt :: String -> Map Gpr MemCounters -> Map Gpr MemCounters
updateCountersOnWaitcnt waitExpr = Map.mapMaybe dropCounters
  where
    dropCounters ctrs = if areCountersEmpty newCtrs then Nothing else Just newCtrs
      where
        newCtrs = ctrs {ctrVMemLoad = counterAfterWait ctrVMemLoad, ctrVMemStore = counterAfterWait ctrVMemStore}
        counterAfterWait ctrTy = case (ctrTy ctrs, ctrTy countersWaitedFor) of
          (Just c, Just w) | c >= w -> Nothing
          (Just c, _) -> Just c
          _ -> Nothing
    countersWaitedFor = foldl' parseCounter (memCounters 0) (split (dropBlanks $ oneOf " &") waitExpr)
    parseCounter :: MemCounters -> String -> MemCounters
    parseCounter ctrs expr
      | [[_, c]] <- expr =~ "vmcnt\\(([0-9]+)\\)" = ctrs {ctrVMemLoad = Just (read c)}
      | otherwise = ctrs

updateCountersOnNewMemoryOp :: MemCounters -> Map Gpr MemCounters -> Map Gpr MemCounters
updateCountersOnNewMemoryOp opCtrs = Map.map $ \ctrs ->
  let inc getCounter = (+ (if isJust (getCounter opCtrs) then 1 else 0)) <$> getCounter ctrs
   in ctrs {ctrVMemLoad = inc ctrVMemLoad, ctrVMemStore = inc ctrVMemStore}

extractMemoryDstAndCounters :: (PC, Instruction) -> Maybe ([Gpr], MemCounters)
extractMemoryDstAndCounters (pc, Instruction opcode (Ovgpr vdst : _))
  | opcode =~ "buffer_(load|atomic)_" = Just (Vgpr <$> vdst, (memCounters pc) {ctrVMemLoad = Just 0})
  | opcode =~ "buffer_store_" = Just (Vgpr <$> vdst, (memCounters pc) {ctrVMemStore = Just 0})
  | otherwise = Nothing
extractMemoryDstAndCounters _ = Nothing
