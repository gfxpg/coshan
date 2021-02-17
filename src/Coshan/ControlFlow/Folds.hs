module Coshan.ControlFlow.Folds where

import Coshan.ControlFlow.Types
import Coshan.Disassembler
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type CallerMap = Map SgprPair BasicBlockIdx

-- | A depth-first preorder traversal of successors, starting at the BB with the given index.
-- The second accumulator ('b') is passed to successors only (does not bubble up).
-- When the second accumulator is Nothing, the successors of the current block are not followed.
foldlWithSuccessors' :: (a -> b -> (BasicBlock, BasicBlockIdx) -> (a, Maybe b)) -> a -> b -> BasicBlockIdx -> CFG -> a
foldlWithSuccessors' f acc tacc bbIdx (CFG bbs) = go (acc, (tacc, Map.empty)) bbIdx
  where
    go (acc, (tacc, callerMap)) bbIdx =
      let bb = bbs !! bbIdx
       in case f acc tacc (bb, bbIdx) of
            (acc', Nothing) -> acc'
            (acc', Just tacc') -> case bbExit bb of
              BbExitFallThrough nextIdx ->
                go (acc', (tacc', callerMap)) nextIdx
              BbExitJump nextIdx ->
                go (acc', (tacc', callerMap)) nextIdx
              BbExitCondJump nextIdx1 nextIdx2 ->
                let acc'' = go (acc', (tacc', callerMap)) nextIdx1
                 in go (acc'', (tacc', callerMap)) nextIdx2
              BbExitJumpSavePc sgprPair nextIdx ->
                let callerMap' = Map.insert sgprPair bbIdx callerMap
                 in go (acc', (tacc', callerMap')) nextIdx
              BbExitDynamic sgprPair
                | Just callerIdx <- Map.lookup sgprPair callerMap ->
                  go (acc', (tacc', callerMap)) (callerIdx + 1)
              _ -> acc'
