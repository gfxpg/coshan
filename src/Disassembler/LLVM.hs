{-# LANGUAGE ForeignFunctionInterface #-}

module Disassembler.LLVM (LLVMDisasmContextRef, getLlvmRef, disassemble) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Internal as BStrInternal
import Data.Word
import Disassembler.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

type LLVMDisasmContextRef = ForeignPtr ()

type LLVMDisasmContextRefRaw = Ptr ()

type UnusuedRef = Ptr ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAMDGPUTargetInfo" llvmInitializeAMDGPUTargetInfo :: IO ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAMDGPUTargetMC" llvmInitializeAMDGPUTargetMC :: IO ()

foreign import ccall "llvm-c/Target.h LLVMInitializeAMDGPUDisassembler" llvmInitializeAMDGPUDisassembler :: IO ()

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
  triple <- newCString $ disasmTriple target
  cpu <- newCString $ disasmCPU target
  ctxRefRaw <- llvmCreateDisasmCPU triple cpu nullPtr 0 nullPtr nullPtr
  ctxRef <- newForeignPtr llvmDisasmDispose ctxRefRaw
  pure $ ctxRef

disassemble :: LLVMDisasmContextRef -> ByteString -> IO [Instruction]
disassemble ctxRef mcodeStr =
  withForeignPtr mcodePtr $ \mcode -> do
    withForeignPtr ctxRef $ \ctx -> do
      outBufRef <- mallocForeignPtrBytes 256
      withForeignPtr outBufRef $ \outbuf -> do
        let parseInstruction = \pos -> do
              posInc <- llvmDisasmInstruction ctx (mcode `plusPtr` pos) (fromIntegral $ mcodeLen - pos) 0 outbuf (CSize 256)
              let instStrWithoutLeadingTab = castPtr $ outbuf `plusPtr` 1
              instStr <- BStr.packCString instStrWithoutLeadingTab
              pure $ (pos + (fromIntegral posInc), instStr)
        let parse = \pos acc ->
              if pos < mcodeLen
                then
                  parseInstruction pos >>= \(nextPos, inst) ->
                    parse nextPos ((fromIntegral pos, inst) : acc)
                else pure $ reverse acc
        parse 0 []
  where
    mcodePtr = mcodeBufPtr `plusForeignPtr` mcodeOffset
    (mcodeBufPtr, mcodeOffset, mcodeLen) = BStrInternal.toForeignPtr mcodeStr
