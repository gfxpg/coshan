module ControlFlow.GeneralSpec where

import ControlFlow
import qualified Data.ByteString as BStr
import Data.Tuple.Strict (mapSnd)
import Disassembler
import Test.Hspec

spec :: Spec
spec = describe "cfg construction" $ do
  it "handles primitive loops" $ do
    elf <- BStr.readFile "test/ControlFlow/Cases/loop-with-cond.hsaco"
    kernels <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    let instructions = mapSnd parseInstruction <$> disasmInstructions kernels
    buildCfg instructions
      `shouldBe` CFG
        [ BasicBlock {bbStartPc = 0, bbEndPc = 24, bbPredecessors = [], bbSuccessors = [1]},
          BasicBlock {bbStartPc = 28, bbEndPc = 40, bbPredecessors = [0, 1, 4], bbSuccessors = [1, 2]},
          BasicBlock {bbStartPc = 44, bbEndPc = 56, bbPredecessors = [1], bbSuccessors = [3]},
          BasicBlock {bbStartPc = 60, bbEndPc = 80, bbPredecessors = [2, 3], bbSuccessors = [3, 4]},
          BasicBlock {bbStartPc = 84, bbEndPc = 100, bbPredecessors = [3], bbSuccessors = [1]}
        ]
