{-# LANGUAGE ForeignFunctionInterface #-}

module Coshan.Disassembler.LLVM (LLVMDisasmContextRef, getLlvmRef, disassemble) where

import Coshan.Disassembler.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Internal
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

type LLVMDisasmContextRef = ForeignPtr ()

type LLVMDisasmContextRefRaw = Ptr ()

type UnusuedRef = Ptr ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAMDGPUTargetInfo"
  llvmInitializeAMDGPUTargetInfo :: IO ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAMDGPUTargetMC"
  llvmInitializeAMDGPUTargetMC :: IO ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAMDGPUDisassembler"
  llvmInitializeAMDGPUDisassembler :: IO ()

foreign import ccall "llvm-c/Disassembler.h LLVMCreateDisasmCPU"
  llvmCreateDisasmCPU :: CString -> CString -> UnusuedRef -> CInt -> UnusuedRef -> UnusuedRef -> IO LLVMDisasmContextRefRaw

foreign import ccall "llvm-c/Disassembler.h &LLVMDisasmDispose"
  llvmDisasmDispose :: FunPtr (LLVMDisasmContextRefRaw -> IO ())

foreign import ccall "llvm-c/Disassembler.h LLVMDisasmInstruction"
  llvmDisasmInstruction :: LLVMDisasmContextRefRaw -> Ptr Word8 -> Word64 -> Word64 -> Ptr Word8 -> CSize -> IO CSize

getLlvmRef :: DisasmTarget -> IO LLVMDisasmContextRef
getLlvmRef target = do
  llvmInitializeAMDGPUTargetInfo
  llvmInitializeAMDGPUTargetMC
  llvmInitializeAMDGPUDisassembler
  withCString (disasmTriple target) $ \tripleStr ->
    withCString (disasmCPU target) $ \cpuStr -> do
      ctxRefRaw <- llvmCreateDisasmCPU tripleStr cpuStr nullPtr 0 nullPtr nullPtr
      newForeignPtr llvmDisasmDispose ctxRefRaw

-- When an invalid instruction is encountered, Left (command address) is returned.
disassemble :: LLVMDisasmContextRef -> ByteString -> IO (Either PC [(PC, ByteString)])
disassemble ctxRef mcodeStr =
  withForeignPtr mcodePtr $ \mcode ->
    withForeignPtr ctxRef $ \ctx ->
      allocaBytes 256 $ \outbuf ->
        let parse pc acc
              | pc >= mcodeLen = return $ Right $ reverse acc
              | otherwise = do
                let mcodeAtPc = mcode `plusPtr` pc
                    mcodeAfterPcLen = fromIntegral $ mcodeLen - pc
                instLen <- llvmDisasmInstruction ctx mcodeAtPc mcodeAfterPcLen 0 outbuf (CSize 256)
                case instLen of
                  0 -> return $ Left pc
                  _ -> do
                    let nextPc = pc + fromIntegral instLen
                        instructionWithoutLeadingTab = castPtr $ outbuf `plusPtr` 1
                    instruction <- BC8.packCString instructionWithoutLeadingTab
                    parse nextPc ((fromIntegral pc, instruction) : acc)
         in parse 0 []
  where
    (mcodePtr, mcodeLen) = Data.ByteString.Internal.toForeignPtr0 mcodeStr
