module Coshan.Reporting (LogMessage (..), LogSpan (..)) where

import Coshan.Disassembler (Operand, PC)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data LogMessage = LogMessage PC [LogSpan]
  deriving (Eq, Show)

data LogSpan
  = LogText String
  | LogInstruction String
  | LogOperand Operand
  | LogInstructionPath [PC]
  deriving (Eq, Show)
