module Coshan.ControlFlow (buildCfg, CFG (..), BasicBlock (..), BasicBlockIdx) where

import Coshan.Disassembler (Instruction (..), Operand (..), PC)
import Data.List (find, findIndex, findIndices, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type BasicBlockIdx = Int

data BasicBlock = BasicBlock {bbInstructions :: [(PC, Instruction)], bbPredecessors :: [BasicBlockIdx], bbSuccessors :: [BasicBlockIdx]}
  deriving (Eq, Show)

newtype CFG = CFG [BasicBlock]
  deriving (Eq, Show)

-- TODO: Build CFG during disassembly: when parsing an instruction, check if it's a branch, and if so, follow it
-- instead of assuming that each succeeding instruction is valid.

buildCfg :: [(PC, Instruction)] -> CFG
buildCfg instrs = CFG blocks''
  where
    blocks'' = (\(bbIdx, bb) -> bb {bbPredecessors = findIndices (elem bbIdx . bbSuccessors) blocks'}) <$> zip [0 ..] blocks'
    blocks' = {- trace ("branches: " ++ show branches) $-} fillSuccs <$> blocks
      where
        fillSuccs (BasicBlock instructions _ _) = BasicBlock instructions [] succBbs
          where
            (bbEndPc, _) = last instructions
            succBbs = case Map.lookup bbEndPc branches of
              Just branchTargets ->
                findIndices (\(BasicBlock ((startPc, _) : _) _ _) -> startPc `Set.member` branchTargets) blocks
              _ -> case findIndex (\(BasicBlock ((startPc, _) : _) _ _) -> startPc > bbEndPc) blocks of
                Just immediateSucc -> [immediateSucc]
                _ -> []
    blocks = {- trace ("starts: " ++ show blockStarts) $-} reverse $ go [] blockStarts instrs
      where
        go bbs _ [] = bbs
        go bbs [_] is = BasicBlock is [] [] : bbs
        go bbs (_ : nextStartPc : rest) is =
          let (currInsts, nextBbInsts) = break ((== nextStartPc) . fst) is
           in go (BasicBlock currInsts [] [] : bbs) (nextStartPc : rest) nextBbInsts
        go _ _ _ = undefined
    blockStarts = Set.toAscList (Map.foldlWithKey' (\brs sourcePc targets -> Set.insert (sourcePc + 4) $ Set.union targets brs) (Set.singleton 0) branches)
    branches = scanBranches instrs

scanBranches :: [(PC, Instruction)] -> Map PC (Set PC)
scanBranches = resolveCallInstructions . scanBranchInstructions

newtype SgprPair = SgprPair (Int, Int) deriving (Eq, Ord, Show)

data Branches = Branches
  { branchJumps :: Map PC (Set PC),
    branchCalls :: [(PC, PC, SgprPair)],
    branchReturns :: [(PC, SgprPair)]
  }

resolveCallInstructions :: Branches -> Map PC (Set PC)
resolveCallInstructions branches = foldl' addCall (branchJumps branches) (branchCalls branches)
  where
    addCall acc (callPc, targetPc, callGprs) =
      Map.insertWith Set.union callPc (Set.singleton targetPc) (followCall acc targetPc)
      where
        pcAfterExit = callPc + 4
        followCall acc currentPc =
          let closestRet = find (\(retPc, retGprs) -> retPc >= currentPc && retGprs == callGprs) (branchReturns branches)
              closestBr = Map.lookupGE currentPc (branchJumps branches)
           in case (closestRet, closestBr) of
                (Nothing, _) ->
                  acc -- TODO: this should probably be a warning (no s_setpc following s_call)
                (Just (retPc, _), Just (brPc, brTargets))
                  | retPc > brPc ->
                    Set.foldl' followCall acc brTargets
                (Just (retPc, _), _) ->
                  Map.insertWith Set.union retPc (Set.singleton pcAfterExit) acc

scanBranchInstructions :: [(PC, Instruction)] -> Branches
scanBranchInstructions instructions = go instructions Branches {branchJumps = Map.empty, branchCalls = [], branchReturns = []}
  where
    go [] acc = acc
    go ((pc, i) : rest) acc = go rest $ case i of
      Instruction opcode [OConst offset] ->
        case opcode of
          ["s", "branch"] ->
            acc {branchJumps = Map.insert pc (Set.singleton (shortJumpTarget pc offset)) (branchJumps acc)}
          "s" : "cbranch" : _ ->
            acc {branchJumps = Map.insert pc (Set.fromList [shortJumpTarget pc offset, pc + 4]) (branchJumps acc)}
          _ ->
            acc
      Instruction ["s", "call", "b64"] [Osgpr [s1, s2], OConst offset] ->
        acc {branchCalls = (pc, shortJumpTarget pc offset, SgprPair (s1, s2)) : branchCalls acc}
      Instruction ["s", "setpc", "b64"] [Osgpr [s1, s2]] ->
        acc {branchReturns = (pc, SgprPair (s1, s2)) : branchReturns acc}
      _ ->
        acc

shortJumpTarget :: PC -> Int -> PC
shortJumpTarget pc offset
  | offset <= 32767 = pc + 4 + 4 * offset
  | otherwise = pc + 4 + 4 * ((-1) * (65536 - offset))
