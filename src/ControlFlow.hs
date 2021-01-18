module ControlFlow (buildCfg, CFG (..), BasicBlock (..)) where

import Analysis
import Data.List (find, findIndex, findIndices, isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Disassembler (Instruction (..), Operand (..), PC)

type BasicBlockIdx = Int

data BasicBlock = BasicBlock {bbStartPc :: PC, bbEndPc :: PC, bbPredecessors :: [BasicBlockIdx], bbSuccessors :: [BasicBlockIdx]}
  deriving (Eq, Show)

newtype CFG = CFG [BasicBlock]
  deriving (Eq, Show)

-- TODO: Build CFG during disassembly: when parsing an instruction, check if it's a branch, and if so, follow it
-- instead of assuming that each succeeding instruction is valid.

buildCfg :: [(PC, Instruction)] -> CFG
buildCfg instrs = CFG blocks''
  where
    blocks'' = (\(bbIdx, bb) -> bb {bbPredecessors = findIndices (elem bbIdx . bbSuccessors) blocks'}) <$> zip [0 ..] blocks'
    blocks' = fillSuccs <$> blocks
      where
        fillSuccs (BasicBlock startPc endPc _ _) = BasicBlock startPc endPc [] succBbs
          where
            succBbs = catMaybes $ immediateSucc : (successor <$> brs)
            immediateSucc
              | not bbEndsWithBranch = findIndex (\(BasicBlock startPc _ _ _) -> startPc > endPc) blocks
              | otherwise = Nothing
            successor (brPc, brTargetPc, _)
              | brPc == endPc = findIndex (\(BasicBlock firstInstPc _ _ _) -> firstInstPc == brTargetPc) blocks
              | otherwise = Nothing
            bbEndsWithBranch = case find ((== endPc) . fst) instrs of
              Just (_, Instruction "s_branch" _) -> True
              Just (_, Instruction opcode _) | "s_cbranch" `isPrefixOf` opcode -> True
              _ -> False
    blocks = reverse $ go [] blockStarts
      where
        blockStarts = Set.toList $ Set.fromList $ 0 : ((\(_, target, _) -> target) <$> brs)
        go bbs [lastBbStart] = let lastPc = fst . last $ instrs in BasicBlock lastBbStart lastPc [] [] : bbs
        go bbs (startPc : nextStartPc : rest) = go (BasicBlock startPc (nextStartPc - 4) [] [] : bbs) (nextStartPc : rest)
    brs = branches instrs

branches :: [(PC, Instruction)] -> [(PC, PC, Bool)] -- (branch instruction pc, branch target pc, taken?)
branches = go []
  where
    go brs [] = brs
    go brs ((pc, i) : instrs) = case i of
      Instruction opcode [OConst offset] ->
        let branchOffset = if offset <= 32767 then offset else -1 * (65536 - offset)
            nextPc = pc + 4
            targetPc = nextPc + branchOffset * 4
         in case opcode of
              "s_branch" ->
                go ((pc, targetPc, True) : brs) instrs
              's' : '_' : 'c' : 'b' : 'r' : 'a' : 'n' : 'c' : 'h' : _ ->
                go ((pc, targetPc, True) : (pc, nextPc, False) : brs) instrs
              _ ->
                go brs instrs
      _ ->
        go brs instrs
