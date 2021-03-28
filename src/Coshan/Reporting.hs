module Coshan.Reporting where

import Coshan.Disassembler (Instruction, Operand, PC)

data LogMessage = LogMessage PC Error
  deriving (Eq, Show)

data Error
  = InstructionRequired
      { instreqInstruction :: Instruction,
        instreqBacktrace :: [(PC, Maybe String)],
        instreqExplanation :: String
      }
  | CounterWaitRequired
      { ctrreqWaitClause :: Operand,
        ctrreqSucceedingEvents :: [(PC, String)],
        ctrreqPrecedingEvents :: [(PC, String)],
        ctrreqExplanation :: String
      }
  deriving (Eq, Show)
