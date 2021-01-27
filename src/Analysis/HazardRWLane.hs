{-# LANGUAGE PatternGuards #-}

module Analysis.HazardRWLane where

import ControlFlow
import Data.Foldable (msum)
import Data.List (find, isPrefixOf)
import Data.Maybe (isJust)
import Disassembler

data LogMessage = LogMessage PC [LogSpan]
  deriving (Eq, Show)

data LogSpan = LogText String | LogInstructionPath [PC]
  deriving (Eq, Show)

checkRwLaneHazards :: DisassembledKernel -> CFG -> [LogMessage]
checkRwLaneHazards _ cfg@(CFG bbs) = go [] bbs
  where
    go log [] = log
    go log (bb : rest) = go (log ++ analyzeBb (CFG bbs) bb) rest

data WaitStatesIterCtx = WaitStatesIterCtx
  { reverseBbInsts :: [(PC, Instruction)], -- instructions we walk over on this iteration
    predBbIdxs :: [BasicBlockIdx], -- predecessors of the current basic block
    walkedInsts :: [(PC, Instruction)], -- all instructions we walked to reach this point (for better messages in logs)
    walkedBbIdxs :: [BasicBlockIdx] -- indexes of all basic blocks we visited (to avoid processing the same blocks in loops)
  }

analyzeBb :: CFG -> BasicBlock -> [LogMessage]
analyzeBb (CFG bbs) currBb = analyzeInstructions ([], bbInstructions currBb) []
  where
    analyzeInstructions (_, []) log = log
    analyzeInstructions (prev, (pc, i) : next) log =
      case i of
        Instruction "v_readlane_b32" [dst, src, Osgpr [selector]]
          | iterCtx <- WaitStatesIterCtx {reverseBbInsts = prev, predBbIdxs = bbPredecessors currBb, walkedInsts = [], walkedBbIdxs = []},
            Just (missingStates, path) <- missingWaitStatesPath 4 selector iterCtx ->
            let missingStatesText = if missingStates == 1 then "1 wait state" else show missingStates ++ " wait states"
                text =
                  [ LogText ("Missing " ++ missingStatesText ++ " for v_readlane_b32 with an SGPR lane selector modified by a VALU instruction:"),
                    LogInstructionPath (fst <$> path)
                  ]
             in analyzeInstructions ((pc, i) : prev, next) (LogMessage pc text : log)
        _ -> analyzeInstructions ((pc, i) : prev, next) log
    missingWaitStatesPath :: Int -> Int -> WaitStatesIterCtx -> Maybe (Int, [(PC, Instruction)]) -- path to the instruction with missing wait states
    missingWaitStatesPath 0 _ _ = Nothing
    missingWaitStatesPath minStates sgprIdx ctx@WaitStatesIterCtx {reverseBbInsts = ((pc, i) : prevInstsInBb)} =
      case i of
        -- Only VOP3B instruction perform VALU operations with an SGPR destination reg
        Instruction opcode [_vdst, Osgpr sdst, _src0, _src1]
          | "v_" `isPrefixOf` opcode,
            sgprIdx `elem` sdst ->
            Just (minStates, (pc, i) : walkedInsts ctx)
        _ ->
          let states =
                case i of
                  Instruction "s_nop" [OConst states] -> 1 + states
                  _ -> 1
           in missingWaitStatesPath (minStates - states) sgprIdx ctx {reverseBbInsts = prevInstsInBb, walkedInsts = (pc, i) : walkedInsts ctx}
    missingWaitStatesPath minStates sgprIdx ctx@WaitStatesIterCtx {reverseBbInsts = [], predBbIdxs = bbIdxs} =
      msum $ walkBb <$> bbIdxs
      where
        walkBb i =
          let bb = bbs !! i
              bbCtx = ctx {reverseBbInsts = reverse $ bbInstructions bb, predBbIdxs = bbPredecessors bb, walkedBbIdxs = i : walkedBbIdxs ctx}
           in missingWaitStatesPath minStates sgprIdx bbCtx
