module Coshan.ControlFlow.Types where

import Coshan.Disassembler (Instruction, PC)

type BasicBlockIdx = Int

data BasicBlock = BasicBlock
  { bbInstructions :: [(PC, Instruction)],
    bbEntries :: [BasicBlockIdx],
    bbExit :: BasicBlockExitPoint BasicBlockIdx
  }
  deriving (Eq, Show)

newtype CFG = CFG [BasicBlock]
  deriving (Eq, Show)

newtype SgprPair = SgprPair (Int, Int)
  deriving (Eq, Ord, Show)

data BasicBlockExitPoint a
  = BbExitJump a
  | BbExitCondJump a a
  | BbExitJumpSavePc SgprPair a
  | BbExitDynamic SgprPair
  | BbExitFallThrough a
  | BbExitTerminal
  deriving (Eq, Show)
