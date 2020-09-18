{-# LANGUAGE ForeignFunctionInterface #-}

module Disassembler where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types

type LLVMDisasmContextRef = ForeignPtr ()
type LLVMDisasmContextRefRaw = Ptr ()
type UnusuedRef = Ptr ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAllTargetInfos"
  llvmInitializeAllTargetInfos :: IO ()

foreign import ccall "llvm-c/Disassembler.h LLVMCreateDisasmCPU"
  llvmCreateDisasmCPU :: CString -> CString -> UnusuedRef -> CInt -> UnusuedRef -> UnusuedRef -> IO LLVMDisasmContextRefRaw

foreign import ccall "llvm-c/Disassembler.h &LLVMDisasmDispose"
  llvmDisasmDispose :: FunPtr (LLVMDisasmContextRefRaw -> IO ())

data DisasmTarget = DisasmTarget { disasmTriple :: String, diasasmCPU :: String }

init :: DisasmTarget -> IO LLVMDisasmContextRef
init target = do
  llvmInitializeAllTargetInfos
  triple <- newCString $ disasmTriple target
  cpu <- newCString $ diasasmCPU target
  ctxRefRaw <- llvmCreateDisasmCPU triple cpu nullPtr 0 nullPtr nullPtr
  ctxRef <- newForeignPtr llvmDisasmDispose ctxRefRaw
  pure $ ctxRef
