module Disassembler.Types where

import Data.ByteString (ByteString)

data DisassembledKernel = DisassembledKernel
  { disasmKernelName :: !ByteString,
    disasmKernelCodeT :: !ByteString,
    disasmInstructionsBin :: !ByteString,
    disasmInstructions :: ![(PC, String)]
  }

data DisasmTarget = DisasmTarget
  { disasmTriple :: String,
    disasmCPU :: String
  }

type PC = Int
