{-# LANGUAGE QuasiQuotes #-}

module Helpers where

import Control.Monad (unless, when)
import Control.Monad.HT (andLazy)
import ControlFlow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.Digest.Pure.MD5 (md5)
import Data.String.Interpolate (i)
import Data.Tuple.Strict (mapSnd)
import Disassembler
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

rocmImage :: String
rocmImage = "rocm/rocm-terminal:4.0"

getTmpDir :: IO String
getTmpDir = (++ "/.test_tmp") <$> getCurrentDirectory

loadGfx900Kernel :: String -> String -> IO (CFG, DisassembledKernel)
loadGfx900Kernel = loadAsmKernel DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}

loadAsmKernel :: DisasmTarget -> String -> String -> IO (CFG, DisassembledKernel)
loadAsmKernel target kernelName kernelText = do
  elf <- compileAsmKernel target kernelName kernelText
  kernel <- head <$> readElf target elf
  let instructions = mapSnd parseInstruction <$> disasmInstructions kernel
      cfg = buildCfg instructions
   in pure (cfg, kernel)

compileAsmKernel :: DisasmTarget -> String -> String -> IO ByteString
compileAsmKernel DisasmTarget {disasmCPU = mcpu} kernelName kernelText = do
  -- TODO: {granulated_}wavefront_sgpr_count, {granulated_}workitem_vgpr_count (via gpr_alloc macros?)
  let source =
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
  let sourceMd5 = md5 $ BC8.pack source
      outputS = kernelName ++ "_" ++ show sourceMd5 ++ ".s"
      outputCo = kernelName ++ "_" ++ show sourceMd5 ++ ".co"
  tmpDir <- getTmpDir
  alreadyCompiled <- doesFileExist (tmpDir ++ "/" ++ outputS) `andLazy` doesFileExist (tmpDir ++ "/" ++ outputCo)
  unless alreadyCompiled $ do
    writeFile (tmpDir ++ "/" ++ outputS) source
    let podmanCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
    let hipccCmd = ["/opt/rocm/llvm/bin/clang", "-x", "assembler", "-target", "amdgcn--amdhsa", "-mcpu=" ++ mcpu, "-mno-code-object-v3", "-o", outputCo, outputS]
    (code, sout, serr) <- readProcessWithExitCode "podman" (podmanCmd ++ hipccCmd) ""
    when (sout /= "") $ putStrLn ("Compiling asm kernel " ++ kernelName ++ ", clang stdout:\n" ++ sout)
    when (serr /= "") $ putStrLn ("Compiling asm kernel " ++ kernelName ++ ", clang stderr:\n" ++ serr)
    when (code /= ExitSuccess) $ do
      fail $ "Failed to compile asm kernel " ++ kernelName ++ ", command:\n" ++ unwords ("podman" : podmanCmd ++ hipccCmd)
  BStr.readFile (tmpDir ++ "/" ++ outputCo)

-- compileHip :: HsaTarget -> String -> IO ByteString
-- compileHip (HsaTarget mcpu) source =
--   writeFile (tmpDir ++ "/hip.cpp") source
--   let podmanCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
--   let hipccCmd = ["/opt/rocm/bin/hipcc", "--genco", "--targets=" ++ mcpu, "-o", "hip.hsaco", "hip.cpp"]
--   sout <- readProcess "podman" (podmanCmd ++ hipccCmd) ""
--   when (sout /= "") $ putStrLn ("HIP compiler output:\n" ++ sout)
--   BStr.readFile (dir ++ "/hip.hsaco")
