{-# LANGUAGE LambdaCase #-}

module Coshan.Analysis.WaitStateHazard (checkWaitStateHazards) where

import Control.Monad.State
import Coshan.ControlFlow
import Coshan.Disassembler
import qualified Coshan.Reporting as R
import Data.Foldable (foldl')

checkWaitStateHazards :: DisassembledKernel -> CFG -> [R.Error]
checkWaitStateHazards _ (CFG bbs) = go [] bbs
  where
    go log [] = log
    go log (bb : rest) = go (log ++ analyzeBb (CFG bbs) bb [rwLaneMatcher]) rest

data WaitStatesIterCtx = WaitStatesIterCtx
  { reverseBbInsts :: [(PC, Instruction)], -- instructions we walk over on this iteration
    predBbIdxs :: [BasicBlockIdx], -- predecessors of the current basic block
    walkedInsts :: [PC] -- all instructions we walked to reach this point (for better messages in logs)
  }

newtype WaitStates = WaitStates Int

type HazardMatcher = Instruction -> Bool

type DependentInstructionMatcher = Instruction -> Maybe (String, WaitStates, HazardMatcher)

rwLaneMatcher :: DependentInstructionMatcher
rwLaneMatcher = \case
  Instruction ("v" : vop : _) [_dst, _src, Osgpr [selectorGprIdx]]
    | vop == "readlane" || vop == "writelane" ->
      Just
        ( "A v_readlane/v_writelane instruction with an SGPR lane selector requires 4 wait states after the selector has been modified by a VALU instruction.",
          WaitStates 4,
          \case
            -- Only VOP3B instructions perform VALU operations with an SGPR destination reg
            Instruction ("v" : _) [_vdst, Osgpr sdst, _src0, _src1] -> selectorGprIdx `elem` sdst
            _ -> False
        )
  _ -> Nothing

analyzeBb :: CFG -> BasicBlock -> [DependentInstructionMatcher] -> [R.Error]
analyzeBb (CFG bbs) currBb matchers = analyzeInstructions ([], bbInstructions currBb) []
  where
    analyzeInstructions (_, []) log = log
    analyzeInstructions (prev, (pc, i) : next) log = analyzeInstructions ((pc, i) : prev, next) log'
      where
        log' = foldl' (\msgs m -> case match m of Just msg -> msg : msgs; _ -> msgs) log matchers
        match m = case m i of
          Just (msg, ws, dp)
            | iterCtx <- WaitStatesIterCtx {reverseBbInsts = prev, predBbIdxs = bbEntries currBb, walkedInsts = []},
              Just (WaitStates missingWs, path) <- missingWaitStatesPath ws dp iterCtx ->
              Just $
                R.Error pc $
                  R.WaitStatesRequired
                    { R.wsreqMissingWaitStates = missingWs,
                      R.wsreqBacktrace = reverse path,
                      R.wsreqExplanation = msg
                    }
          _ -> Nothing
    missingWaitStatesPath :: WaitStates -> HazardMatcher -> WaitStatesIterCtx -> Maybe (WaitStates, [PC]) -- path to the instruction with missing wait states
    missingWaitStatesPath (WaitStates ws) _ _ | ws <= 0 = Nothing
    missingWaitStatesPath (WaitStates ws) dependsOn ctx@WaitStatesIterCtx {reverseBbInsts = ((pc, i) : prevInstsInBb)}
      | dependsOn i = Just (WaitStates ws, pc : walkedInsts ctx)
      | otherwise =
        let states = case i of
              Instruction ["s", "nop"] [OConst states] -> 1 + states
              _ -> 1
         in missingWaitStatesPath (WaitStates $ ws - states) dependsOn ctx {reverseBbInsts = prevInstsInBb, walkedInsts = pc : walkedInsts ctx}
    missingWaitStatesPath ws m ctx@WaitStatesIterCtx {reverseBbInsts = [], predBbIdxs = predBbIdxs} =
      msum $ walkBb <$> predBbIdxs
      where
        walkBb i =
          let BasicBlock {bbInstructions = newInstructions, bbEntries = newPredecessors} = bbs !! i
              bbCtx = ctx {reverseBbInsts = reverse newInstructions, predBbIdxs = newPredecessors}
           in missingWaitStatesPath ws m bbCtx
