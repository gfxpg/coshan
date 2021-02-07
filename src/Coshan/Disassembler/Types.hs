module Coshan.Disassembler.Types where

import Data.ByteString (ByteString)

data Instruction = Instruction String [Operand]
  deriving (Eq, Show, Read)

data Operand
  = Osgpr [Int]
  | Ovgpr [Int]
  | Ottmp [Int]
  | OConst Int
  | OConstF Float
  | OOther String
  | Ovmcnt Int
  | Ovscnt Int
  | Oexpcnt Int
  | Olgkmcnt Int
  deriving (Eq, Show, Read)

data DisassembledKernel = DisassembledKernel
  { disasmKernelName :: !ByteString,
    disasmKernelCodeT :: !ByteString,
    disasmInstructionsBin :: !ByteString,
    disasmInstructions :: ![(PC, ByteString)]
  }
  deriving (Show)

data DisasmTarget = DisasmTarget
  { disasmTriple :: String,
    disasmCPU :: String
  }

type PC = Int
