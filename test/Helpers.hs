{-# LANGUAGE QuasiQuotes #-}

module Helpers where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import Data.String.Interpolate (i)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Directory (getCurrentDirectory)

rocmImage :: String
rocmImage = "rocm/rocm-terminal:4.0"

getTmpDir :: IO String
getTmpDir = (++ "/.test_tmp") <$> getCurrentDirectory

newtype HsaTarget = HsaTarget String

compileAsmKernel :: String -> HsaTarget -> String -> IO ByteString
compileAsmKernel kernelName (HsaTarget mcpu) kernelText = do
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
  tmpDir <- getTmpDir
  writeFile (tmpDir ++ "/" ++ kernelName ++ ".s") source
  let podmanCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
  let hipccCmd = ["/opt/rocm/llvm/bin/clang", "-x", "assembler", "-target", "amdgcn--amdhsa", "-mcpu=gfx900", "-mno-code-object-v3", "-o", kernelName ++ ".co", kernelName ++ ".s"]
  (code, sout, serr) <- readProcessWithExitCode "podman" (podmanCmd ++ hipccCmd) ""
  when (sout /= "") $ putStrLn ("Compiling asm kernel " ++ kernelName ++ ", clang stdout:\n" ++ sout)
  when (serr /= "") $ putStrLn ("Compiling asm kernel " ++ kernelName ++ ", clang stderr:\n" ++ serr)
  when (code /= ExitSuccess) $ do
    fail $ "Failed to compile asm kernel " ++ kernelName ++ ", command:\n" ++ unwords ("podman" : podmanCmd ++ hipccCmd)
  BStr.readFile (tmpDir ++ "/" ++ kernelName ++ ".co")

-- compileHip :: HsaTarget -> String -> IO ByteString
-- compileHip (HsaTarget mcpu) source =
--   writeFile (tmpDir ++ "/hip.cpp") source
--   let podmanCmd = ["run", "--user=root", "-v", tmpDir ++ ":/out:z", "-w=/out", rocmImage]
--   let hipccCmd = ["/opt/rocm/bin/hipcc", "--genco", "--targets=" ++ mcpu, "-o", "hip.hsaco", "hip.cpp"]
--   sout <- readProcess "podman" (podmanCmd ++ hipccCmd) ""
--   when (sout /= "") $ putStrLn ("HIP compiler output:\n" ++ sout)
--   BStr.readFile (dir ++ "/hip.hsaco")
