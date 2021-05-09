module Coshan.Reporting where

import Coshan.Disassembler (Operand, PC)

data Error = Error PC Violation
  deriving (Eq, Show)

data Violation
  = CounterWaitRequired
      { ctrreqWaitcntClause :: Operand,
        ctrreqSucceedingEvents :: [(PC, String)],
        ctrreqPrecedingEvents :: [(PC, String)],
        ctrreqExplanation :: String
      }
  | WaitStatesRequired
      { wsreqMissingWaitStates :: Int,
        wsreqBacktrace :: [PC],
        wsreqExplanation :: String
      }
  deriving (Eq, Show)
