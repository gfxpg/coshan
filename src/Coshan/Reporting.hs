module Coshan.Reporting where

import Coshan.Disassembler (Instruction, PC)

data LogMessage = LogMessage PC Error
  deriving (Eq, Show)

data Error = InstructionRequired
  { instreqInstruction :: Instruction,
    instreqBacktrace :: [PC],
    instreqExplanation :: String
  }
  deriving (Eq, Show)
