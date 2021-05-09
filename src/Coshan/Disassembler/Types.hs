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

data KernelDescriptor = KernelDescriptorV2 !ByteString | KernelDescriptorV3 !ByteString
  deriving (Show)

data DisassembledKernel = DisassembledKernel
  { disasmKernelName :: !ByteString,
    disasmKernelCodeT :: !KernelDescriptor,
    disasmInstructionsBin :: !ByteString,
    disasmInstructions :: ![(PC, ByteString)]
  }
  deriving (Show)

data DisasmTarget = DisasmTarget
  { disasmTriple :: String,
    disasmCPU :: String
  }

type PC = Int

data DisassemblyError = DisasmInvalidInstruction DisassembledKernel PC
