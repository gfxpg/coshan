{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helpers where

import Control.Monad (unless, when)
import Control.Monad.HT (andLazy)
import Coshan.ControlFlow
import Coshan.Disassembler
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.Digest.Pure.MD5 (md5)
import Data.List (intercalate)
import Data.String.Interpolate (i)
import Data.Tuple.Strict (mapSnd)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data CodeObject = CodeObject {coCpu :: String, coMetadataV3 :: Bool, coKernels :: [(String, ByteString)]}

gfx900Kernel :: String -> ByteString -> CodeObject
gfx900Kernel name text = CodeObject {coCpu = "gfx900", coMetadataV3 = False, coKernels = [(name, text)]}

loadFirstKernel :: CodeObject -> IO (CFG, DisassembledKernel)
loadFirstKernel co = do
  elf <- compileCo co
  kernel <- head <$> readElf DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = coCpu co} elf
  let instructions = mapSnd parseInstruction <$> disasmInstructions kernel
      cfg = buildCfg instructions
   in pure (cfg, kernel)

rocmImage :: String
rocmImage = "rocm/rocm-terminal:4.0"

getTmpDir :: IO String
getTmpDir = (++ "/.test_tmp") <$> getCurrentDirectory

compileCo :: CodeObject -> IO ByteString
compileCo co = do
  let source = getCoListing co
      sourceMd5 = md5 source
      kernelNames = intercalate "_" (fst <$> coKernels co)
      outputS = kernelNames ++ "_" ++ show sourceMd5 ++ ".s"
      outputCo = kernelNames ++ "_" ++ show sourceMd5 ++ ".co"
  tmpDir <- getTmpDir
  alreadyCompiled <- doesFileExist (tmpDir ++ "/" ++ outputS) `andLazy` doesFileExist (tmpDir ++ "/" ++ outputCo)
  unless alreadyCompiled $ do
    BLC8.writeFile (tmpDir ++ "/" ++ outputS) source
    let podmanCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
    let metadataFlag = if coMetadataV3 co then "-mcode-object-v3" else "-mno-code-object-v3"
    let hipccCmd = ["/opt/rocm/llvm/bin/clang", "-x", "assembler", "-target", "amdgcn--amdhsa", "-mcpu=" ++ coCpu co, metadataFlag, "-o", outputCo, outputS]
    (code, sout, serr) <- readProcessWithExitCode "podman" (podmanCmd ++ hipccCmd) ""
    let fullCmdString = unwords ("podman" : podmanCmd ++ hipccCmd)
    when (sout /= "") $ putStrLn ("[stdout] " ++ fullCmdString ++ "\n" ++ sout)
    when (serr /= "") $ putStrLn ("[stderr] " ++ fullCmdString ++ "\n" ++ serr)
    when (code /= ExitSuccess) $ fail ("Failed to compile code object " ++ kernelNames)
  BStr.readFile (tmpDir ++ "/" ++ outputCo)

getCoListing :: CodeObject -> BLC8.ByteString
getCoListing co = BLC8.intercalate "\n" sources
  where
    sources = (if coMetadataV3 co then kernelV3 else kernelV2) <$> coKernels co
    kernelV2 :: (String, ByteString) -> BLC8.ByteString
    kernelV2 (kernelName, kernelText) =
      [i|
          .hsa_code_object_version 2,1
          .amdgpu_hsa_kernel #{kernelName}
          .global #{kernelName}
          .text
          .p2align 8
          #{kernelName}:
          .amd_kernel_code_t
          .end_amd_kernel_code_t
          #{kernelText}
        |]
    kernelV3 :: (String, ByteString) -> BLC8.ByteString
    kernelV3 (kernelName, kernelText) =
      [i|
          .global #{kernelName}
          .text
          .p2align 8
          #{kernelName}:
          #{kernelText}
          .rodata
          .p2align 6
          .amdhsa_kernel #{kernelName}
          .amdhsa_next_free_vgpr .amdgcn.next_free_vgpr
          .amdhsa_next_free_sgpr .amdgcn.next_free_sgpr
          .end_amdhsa_kernel
          .set .amdgcn.next_free_vgpr, 0
          .set .amdgcn.next_free_sgpr, 0
        |]

-- compileHip :: HsaTarget -> String -> IO ByteString
-- compileHip (HsaTarget mcpu) source =
--   writeFile (tmpDir ++ "/hip.cpp") source
--   let podmanCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
--   let hipccCmd = ["/opt/rocm/bin/hipcc", "--genco", "--targets=" ++ mcpu, "-o", "hip.hsaco", "hip.cpp"]
--   sout <- readProcess "podman" (podmanCmd ++ hipccCmd) ""
--   when (sout /= "") $ putStrLn ("HIP compiler output:\n" ++ sout)
--   BStr.readFile (dir ++ "/hip.hsaco")
