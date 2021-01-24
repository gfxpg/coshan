{-# LANGUAGE QuasiQuotes #-}

module ControlFlow.GeneralSpec where

import ControlFlow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import Data.String.Interpolate (i)
import Data.Tuple.Strict (mapSnd)
import Disassembler
import Helpers
import Test.Hspec

simpleLoopKernel :: IO ByteString
simpleLoopKernel =
  compileAsmKernel "cfg_simple_loop" (HsaTarget "gfx900") $
    [i|
s_load_dwordx2 s[0:1], s[4:5], 0x0
s_waitcnt lgkmcnt(0)
s_load_dword s2, s[0:1], 0x0
s_waitcnt lgkmcnt(0)
v_mov_b32_e32 v0, s2
bb28:
  v_add_f32_e32 v0, 1.0, v0
  v_cmp_lt_f32_e32 vcc, 1.0, v0
  s_and_b64 vcc, exec, vcc
  s_cbranch_vccnz bb28
bb44:
  s_load_dword s2, s[0:1], 0x4
  s_waitcnt lgkmcnt(0)
  v_mov_b32_e32 v1, s2
bb60:
  v_add_f32_e32 v0, 1.0, v0
  v_cmp_nlt_f32_e32 vcc, 1.0, v0
  v_add_f32_e32 v1, 0x40a00000, v1
  s_and_b64 vcc, exec, vcc
  s_cbranch_vccnz bb60
bb84:
  v_mov_b32_e32 v3, s1
  v_mov_b32_e32 v2, s0
  global_store_dword v[2:3], v1, off offset:4
  s_branch bb28
|]

spec :: Spec
spec = describe "cfg construction" $ do
  it "handles primitive loops" $ do
    kernel <- head <$> (simpleLoopKernel >>= readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}))
    let instructions = mapSnd parseInstruction <$> disasmInstructions kernel
    buildCfg instructions
      `shouldBe` CFG
        [ BasicBlock {bbStartPc = 0, bbEndPc = 24, bbPredecessors = [], bbSuccessors = [1]},
          BasicBlock {bbStartPc = 28, bbEndPc = 40, bbPredecessors = [0, 1, 4], bbSuccessors = [1, 2]},
          BasicBlock {bbStartPc = 44, bbEndPc = 56, bbPredecessors = [1], bbSuccessors = [3]},
          BasicBlock {bbStartPc = 60, bbEndPc = 80, bbPredecessors = [2, 3], bbSuccessors = [3, 4]},
          BasicBlock {bbStartPc = 84, bbEndPc = 100, bbPredecessors = [3], bbSuccessors = [1]}
        ]
