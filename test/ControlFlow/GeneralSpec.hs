{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ControlFlow.GeneralSpec where

import Coshan.ControlFlow
import Coshan.Disassembler
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import Data.String.Interpolate (i)
import Data.Tuple.Strict (mapSnd)
import Helpers
import Test.Hspec

simpleLoopKernel :: IO ByteString
simpleLoopKernel =
  compileAsmKernel DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"} "cfg_simple_loop" $
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
        [ BasicBlock
            { bbInstructions =
                [ (0, Instruction ["s", "load", "dwordx2"] [Osgpr [0, 1], Osgpr [4, 5], OConst 0]),
                  (8, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (12, Instruction ["s", "load", "dword"] [Osgpr [2], Osgpr [0, 1], OConst 0]),
                  (20, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (24, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [0], Osgpr [2]])
                ],
              bbPredecessors = [],
              bbSuccessors = [1]
            },
          BasicBlock
            { bbInstructions =
                [ (28, Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 1.0, Ovgpr [0]]),
                  (32, Instruction ["v", "cmp", "lt", "f32", "e32"] [OCtrl "vcc", OConstF 1.0, Ovgpr [0]]),
                  (36, Instruction ["s", "and", "b64"] [OCtrl "vcc", OCtrl "exec", OCtrl "vcc"]),
                  (40, Instruction ["s", "cbranch", "vccnz"] [OConst 65532])
                ],
              bbPredecessors = [0, 1, 4],
              bbSuccessors = [1, 2]
            },
          BasicBlock
            { bbInstructions =
                [ (44, Instruction ["s", "load", "dword"] [Osgpr [2], Osgpr [0, 1], OConst 4]),
                  (52, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (56, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [1], Osgpr [2]])
                ],
              bbPredecessors = [1],
              bbSuccessors = [3]
            },
          BasicBlock
            { bbInstructions =
                [ (60, Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 1.0, Ovgpr [0]]),
                  (64, Instruction ["v", "cmp", "nlt", "f32", "e32"] [OCtrl "vcc", OConstF 1.0, Ovgpr [0]]),
                  (68, Instruction ["v", "add", "f32", "e32"] [Ovgpr [1], OConst 1084227584, Ovgpr [1]]),
                  (76, Instruction ["s", "and", "b64"] [OCtrl "vcc", OCtrl "exec", OCtrl "vcc"]),
                  (80, Instruction ["s", "cbranch", "vccnz"] [OConst 65530])
                ],
              bbPredecessors = [2, 3],
              bbSuccessors = [3, 4]
            },
          BasicBlock
            { bbInstructions =
                [ (84, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [3], Osgpr [1]]),
                  (88, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [2], Osgpr [0]]),
                  (92, Instruction ["global", "store", "dword"] [Ovgpr [2, 3], Ovgpr [1], OCtrl "off", OCtrl "offset:4"]),
                  (100, Instruction ["s", "branch"] [OConst 65517])
                ],
              bbPredecessors = [3],
              bbSuccessors = [1]
            }
        ]
