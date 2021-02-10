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
import Numeric (readHex)
import System.Console.CmdArgs

data Opts = Opts
  { optCpu, optTriple :: String,
    optCo :: FilePath,
    optDisasm :: Bool,
    optNops :: [String]
  }
  deriving (Data, Typeable, Show)

opts :: Opts
opts =
  Opts
    { optCpu = "gfx900" &= explicit &= name "mcpu" &= typ "gfx900" &= help "Target CPU type for disassembly (run `llc -march=amdgcn -mcpu=help` for a list of available CPUs)",
      optTriple = "amdgcn--amdhsa" &= explicit &= name "triple" &= typ "amdgcn--amdhsa" &= help "Target triple for disassembly",
      optCo = def &= argPos 0 &= typFile,
      optDisasm = def &= explicit &= name "disasm" &= help "Print disassembled shader code and exit",
      optNops = def &= explicit &= name "nop" &= help "Replace all DWORDs of instructions at specified locations (hex) with `s_nop 0`. LIMITATIONS: does not affect the binary form of instructions."
    }
    &= program "coshan"
    &= summary "compute shader analyzer 0.1"
    &= help "Disassemble and analyze compute shaders"

main :: IO ()
main = do
  args <- cmdArgs opts

  let target = DisasmTarget {disasmTriple = optTriple args, disasmCPU = optCpu args}
  let nopLocations = fst . head . readHex <$> optNops args

  co <- B.readFile (optCo args)
  kernel <- insertNops nopLocations . head <$> readElf target co

  let instructions = second parseInstruction <$> disasmInstructions kernel
  let cfg = buildCfg instructions

  case args of
    Opts {optDisasm = True} -> do
      printCfg kernel cfg
    _ -> do
      report "waitcnt" (checkWaitcnts kernel cfg)
      report "s_nop" (checkRwLaneHazards kernel cfg)

insertNops :: [Int] -> DisassembledKernel -> DisassembledKernel
insertNops [] kernel = kernel
insertNops nops kernel = kernel {disasmInstructions = scanInstructions (disasmInstructions kernel) []}
  where
    scanInstructions [] acc = reverse acc
    scanInstructions ((pc, i) : rest) acc
      | pc `elem` nops = scanInstructions rest $ case rest of
        (nextPc, _) : _ -> ((,"s_nop 0") <$> [nextPc - 4, nextPc - 8 .. pc]) ++ acc
        _ -> (pc, "s_nop 0") : acc
      | otherwise = scanInstructions rest $ (pc, i) : acc

report :: String -> [LogMessage] -> IO ()
report analyzer [] = putStrLn (analyzer ++ ": OK")
report analyzer log = putStrLn (analyzer ++ ":") >> print log
