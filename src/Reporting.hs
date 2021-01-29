module Reporting where

import Disassembler (PC)

data LogMessage = LogMessage PC [LogSpan]
  deriving (Eq, Show)

data LogSpan = LogText String | LogInstructionPath [PC]
  deriving (Eq, Show)
