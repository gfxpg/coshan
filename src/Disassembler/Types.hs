module Disassembler.Types where

import Data.ByteString (ByteString)

data DisassembledKernel = DisassembledKernel
  { disasmKernelName :: !ByteString,
    disasmKernelCodeT :: !ByteString,
    disasmInstructionsBin :: !ByteString,
    disasmInstructions :: ![(PC, ByteString)]
  }

data DisasmTarget = DisasmTarget
  { disasmTriple :: String,
    disasmCPU :: String
  }

type PC = Int
