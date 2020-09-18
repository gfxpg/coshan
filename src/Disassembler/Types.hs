module Disassembler.Types where

import Data.ByteString (ByteString)

data Disassembly = Disassembly
  { disasmKernelCode :: ByteString,
    disasmInstructionsBin :: ByteString,
    disasmInstructions :: [Instruction]
  }

data DisasmTarget = DisasmTarget
  { disasmTriple :: String,
    disasmCPU :: String
  }

type PC = Int

type Instruction = (PC, ByteString)
