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
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data CodeObject = CodeObject {coCpu :: String, coMetadataVersion :: Int, coKernels :: [(String, ByteString)]}

gfx908Kernel :: String -> ByteString -> CodeObject
gfx908Kernel name text = CodeObject {coCpu = "gfx908", coMetadataVersion = 3, coKernels = [(name, text)]}

extractFirstKernel :: CodeObject -> IO DisassembledKernel
extractFirstKernel co = do
  elf <- compileCo co
  elfContents <- readElf DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = coCpu co} elf
  case elfContents of
    Left (DisasmInvalidInstruction k pc) -> error $ "Unable to disassemble " <> show (disasmKernelName k) <> ": invalid instruction at PC = " <> show pc
    Right (firstKernel : _) -> return firstKernel

loadFirstKernel :: CodeObject -> IO (CFG, DisassembledKernel)
loadFirstKernel co = do
  kernel <- extractFirstKernel co
  let instructions = mapSnd parseInstruction <$> disasmInstructions kernel
  return (buildCfg instructions, kernel)

rocmImage :: String
rocmImage = "rocm/rocm-terminal:4.1.1"

getTmpDir :: IO String
getTmpDir = do
  path <- (++ "/.test_tmp") <$> getCurrentDirectory
  exists <- doesDirectoryExist path
  unless exists $ createDirectory path
  return path

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
    let dockerCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
    let ccCmd =
          [ "/opt/rocm/llvm/bin/clang",
            "-x",
            "assembler",
            "-target",
            "amdgcn--amdhsa",
            "-mcpu=" ++ coCpu co,
            "-mcode-object-version=" ++ show (coMetadataVersion co),
            "-o",
            outputCo,
            outputS
          ]
    (code, sout, serr) <- readProcessWithExitCode "docker" (dockerCmd ++ ccCmd) ""
    let fullCmdString = unwords ("docker" : dockerCmd ++ ccCmd)
    when (sout /= "") $ putStrLn ("[stdout] " ++ fullCmdString ++ "\n" ++ sout)
    when (serr /= "") $ putStrLn ("[stderr] " ++ fullCmdString ++ "\n" ++ serr)
    when (code /= ExitSuccess) $ fail ("Failed to compile code object " ++ kernelNames)
  BStr.readFile (tmpDir ++ "/" ++ outputCo)

getCoListing :: CodeObject -> BLC8.ByteString
getCoListing co = BLC8.intercalate "\n" sources
  where
    sources = (if coMetadataVersion co == 3 then kernelV3 else kernelV2) <$> coKernels co
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
