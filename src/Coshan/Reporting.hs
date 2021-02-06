module Coshan.Reporting (LogMessage (..), LogSpan (..)) where

import Coshan.Disassembler (Operand, PC)

data LogMessage = LogMessage PC [LogSpan]
  deriving (Eq, Show)

data LogSpan
  = LogText String
  | LogInstruction String
  | LogOperand Operand
  | LogInstructionPath [PC]
  deriving (Eq, Show)
