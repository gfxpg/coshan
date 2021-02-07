module Coshan.Disassembler.Types where

import Data.ByteString (ByteString)

data Instruction = Instruction [ByteString] [Operand]
  deriving (Eq, Show, Read)

data Operand
  = Osgpr [Int]
  | Ovgpr [Int]
  | Ottmp [Int]
  | Ovmcnt Int
  | Oexpcnt Int
  | Olgkmcnt Int
  | OConst Int
  | OConstF Float
  | OCtrl ByteString
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
