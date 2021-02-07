{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Coshan.Analysis.HazardRWLane
import Coshan.Analysis.Waitcnt
import Coshan.ControlFlow
import Coshan.Disassembler
import Coshan.Reporting
import Data.Bifunctor (second)
import qualified Data.ByteString as B
import Format.Cfg (printCfg)
import System.Console.CmdArgs

data Opts = Opts
  { optCpu, optTriple :: String,
    optCo :: FilePath,
    optDumpCfg :: Bool
  }
  deriving (Data, Typeable, Show)

opts :: Opts
opts =
  Opts
    { optCpu = "gfx900" &= explicit &= name "mcpu" &= typ "gfx900" &= help "Target CPU type for disassembly (run `llc -march=amdgcn -mcpu=help` for a list of available CPUs)",
      optTriple = "amdgcn--amdhsa" &= explicit &= name "triple" &= typ "amdgcn--amdhsa" &= help "Target triple for disassembly",
      optDumpCfg = def &= explicit &= name "dump-cfg" &= help "Print reconstructed CFG and exit",
      optCo = def &= argPos 0 &= typFile
    }
    &= program "coshan"
    &= summary "compute shader analyzer 0.1"
    &= help "Disassemble and analyze compute shaders"

main :: IO ()
main = do
  args <- cmdArgs opts

  let target = DisasmTarget {disasmTriple = optTriple args, disasmCPU = optCpu args}

  co <- B.readFile (optCo args)
  kernel <- head <$> readElf target co

  let instructions = second parseInstruction <$> disasmInstructions kernel
  let cfg = buildCfg instructions

  case args of
    Opts {optDumpCfg = True} -> do
      printCfg cfg
    _ -> do
      report "waitcnt" (checkWaitcnts kernel cfg)
      report "s_nop" (checkRwLaneHazards kernel cfg)

report :: String -> [LogMessage] -> IO ()
report analyzer [] = putStrLn (analyzer ++ ": OK")
report analyzer log = putStrLn (analyzer ++ ":") >> print log
