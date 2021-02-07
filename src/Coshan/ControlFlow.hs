{-# LANGUAGE OverloadedStrings #-}

module Coshan.ControlFlow (buildCfg, CFG (..), BasicBlock (..), BasicBlockIdx) where

import Coshan.Disassembler (Instruction (..), Operand (..), PC)
import Data.List (findIndex, findIndices)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

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
    blocks' = fillSuccs <$> blocks
      where
        fillSuccs (BasicBlock instructions _ _) = BasicBlock instructions [] succBbs
          where
            (bbEndPc, bbLastInstr) = last instructions
            succBbs = catMaybes $ immediateSucc : (successor <$> brs)
            immediateSucc
              | not bbEndsWithBranch = findIndex (\(BasicBlock ((startPc, _) : _) _ _) -> startPc > bbEndPc) blocks
              | otherwise = Nothing
            successor (brPc, brTargetPc, _)
              | brPc == bbEndPc = findIndex (\(BasicBlock ((startPc, _) : _) _ _) -> startPc == brTargetPc) blocks
              | otherwise = Nothing
            bbEndsWithBranch = case bbLastInstr of
              Instruction ["s", "branch"] _ -> True
              Instruction ("s" : "cbranch" : _) _ -> True
              _ -> False
    blocks = reverse $ go [] blockStarts instrs
      where
        blockStarts = Set.toList $ Set.fromList $ 0 : ((\(_, target, _) -> target) <$> brs)
        go bbs [_] is = BasicBlock is [] [] : bbs
        go bbs (_ : nextStartPc : rest) is =
          let (currInsts, nextBbInsts) = break ((== nextStartPc) . fst) is
           in go (BasicBlock currInsts [] [] : bbs) (nextStartPc : rest) nextBbInsts
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
              ["s", "branch"] ->
                go ((pc, targetPc, True) : brs) instrs
              "s" : "cbranch" : _ ->
                go ((pc, targetPc, True) : (pc, nextPc, False) : brs) instrs
              _ ->
                go brs instrs
      _ ->
        go brs instrs
