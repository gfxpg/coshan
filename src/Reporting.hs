module Reporting (LogMessage(..), LogSpan(..)) where

import Disassembler (Operand, PC)

data LogMessage = LogMessage PC [LogSpan]
  deriving (Eq, Show)

data LogSpan
  = LogText String
  | LogInstruction String
  | LogOperand Operand
  | LogInstructionPath [PC]
  deriving (Eq, Show)
