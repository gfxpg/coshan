{-# LANGUAGE QuasiQuotes #-}

module ControlFlow.GeneralSpec where

import Coshan.ControlFlow
import Coshan.ControlFlow.Types
import Coshan.Disassembler
import Data.String.Interpolate (i)
import Helpers
import Test.Hspec

spec :: Spec
spec = describe "cfg construction" $ do
  it "recognizes scalar branching, including loops" $ do
    (cfg, _) <-
      loadFirstKernel . gfx908Kernel "cfg_simple_loop" $
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
    cfg
      `shouldBe` CFG
        [ BasicBlock
            { bbInstructions =
                [ (0, Instruction ["s", "load", "dwordx2"] [Osgpr [0, 1], Osgpr [4, 5], OConst 0]),
                  (8, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (12, Instruction ["s", "load", "dword"] [Osgpr [2], Osgpr [0, 1], OConst 0]),
                  (20, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (24, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [0], Osgpr [2]])
                ],
              bbEntries = [],
              bbExit = BbExitFallThrough 1
            },
          BasicBlock
            { bbInstructions =
                [ (28, Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 1.0, Ovgpr [0]]),
                  (32, Instruction ["v", "cmp", "lt", "f32", "e32"] [OCtrl "vcc", OConstF 1.0, Ovgpr [0]]),
                  (36, Instruction ["s", "and", "b64"] [OCtrl "vcc", OCtrl "exec", OCtrl "vcc"]),
                  (40, Instruction ["s", "cbranch", "vccnz"] [OConst 65532])
                ],
              bbEntries = [0, 1, 4],
              bbExit = BbExitCondJump 1 2
            },
          BasicBlock
            { bbInstructions =
                [ (44, Instruction ["s", "load", "dword"] [Osgpr [2], Osgpr [0, 1], OConst 4]),
                  (52, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (56, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [1], Osgpr [2]])
                ],
              bbEntries = [1],
              bbExit = BbExitFallThrough 3
            },
          BasicBlock
            { bbInstructions =
                [ (60, Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 1.0, Ovgpr [0]]),
                  (64, Instruction ["v", "cmp", "nlt", "f32", "e32"] [OCtrl "vcc", OConstF 1.0, Ovgpr [0]]),
                  (68, Instruction ["v", "add", "f32", "e32"] [Ovgpr [1], OConst 1084227584, Ovgpr [1]]),
                  (76, Instruction ["s", "and", "b64"] [OCtrl "vcc", OCtrl "exec", OCtrl "vcc"]),
                  (80, Instruction ["s", "cbranch", "vccnz"] [OConst 65530])
                ],
              bbEntries = [2, 3],
              bbExit = BbExitCondJump 3 4
            },
          BasicBlock
            { bbInstructions =
                [ (84, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [3], Osgpr [1]]),
                  (88, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [2], Osgpr [0]]),
                  (92, Instruction ["global", "store", "dword"] [Ovgpr [2, 3], Ovgpr [1], OCtrl "off", OCtrl "offset:4"]),
                  (100, Instruction ["s", "branch"] [OConst 65517])
                ],
              bbEntries = [3],
              bbExit = BbExitJump 1
            }
        ]

  it "recognizes function calls that use s_call_b64" $ do
    (cfg, _) <-
      loadFirstKernel . gfx908Kernel "cfg_s_call" $
        [i|
          bb0:
          s_load_dwordx2 s[0:1], s[4:5], 0x0
          s_call_b64 s[10:11], fun1
          v_cmp_lt_f32_e32 vcc, 1.0, v0
          s_cbranch_vccnz bb0

          bb1:
          global_store_dword v[2:3], v0, off offset:4
          s_call_b64 s[10:11], fun1
          s_call_b64 s[0:1], fun2
          s_cmp_eq_u32 s0, 0
          s_cbranch_scc0 bb0
          s_branch end

          fun2:
          s_setpc_b64 s[0:1]

          fun1:
          s_waitcnt lgkmcnt(0)
          s_cmp_eq_u32 s1, 0
          s_cbranch_scc1 fun1_end
          s_call_b64 s[0:1], fun2
          v_mov_b32_e32 v0, s1
          v_add_f32_e32 v0, 1.0, v0
          fun1_end:
          s_setpc_b64 s[10:11]

          end:
          s_endpgm
        |]
    cfg
      `shouldBe` CFG
        [ BasicBlock -- 0
            { bbInstructions =
                [ (0, Instruction ["s", "load", "dwordx2"] [Osgpr [0, 1], Osgpr [4, 5], OConst 0]),
                  (8, Instruction ["s", "call", "b64"] [Osgpr [10, 11], OConst 10])
                ],
              bbEntries = [1, 4],
              bbExit = BbExitJumpSavePc (SgprPair (10, 11)) 7
            },
          BasicBlock -- 1
            { bbInstructions =
                [ (12, Instruction ["v", "cmp", "lt", "f32", "e32"] [OCtrl "vcc", OConstF 1.0, Ovgpr [0]]),
                  (16, Instruction ["s", "cbranch", "vccnz"] [OConst 65531])
                ],
              bbEntries = [10],
              bbExit = BbExitCondJump 0 2
            },
          BasicBlock -- 2
            { bbInstructions =
                [ (20, Instruction ["global", "store", "dword"] [Ovgpr [2, 3], Ovgpr [0], OCtrl "off", OCtrl "offset:4"]),
                  (28, Instruction ["s", "call", "b64"] [Osgpr [10, 11], OConst 5])
                ],
              bbEntries = [1],
              bbExit = BbExitJumpSavePc (SgprPair (10, 11)) 7
            },
          BasicBlock -- 3
            { bbInstructions = [(32, Instruction ["s", "call", "b64"] [Osgpr [0, 1], OConst 3])],
              bbEntries = [10],
              bbExit = BbExitJumpSavePc (SgprPair (0, 1)) 6
            },
          BasicBlock -- 4
            { bbInstructions =
                [ (36, Instruction ["s", "cmp", "eq", "u32"] [Osgpr [0], OConst 0]),
                  (40, Instruction ["s", "cbranch", "scc0"] [OConst 65525])
                ],
              bbEntries = [6],
              bbExit = BbExitCondJump 0 5
            },
          BasicBlock -- 5
            { bbInstructions = [(44, Instruction ["s", "branch"] [OConst 8])],
              bbEntries = [4],
              bbExit = BbExitJump 11
            },
          BasicBlock -- 6
            { bbInstructions = [(48, Instruction ["s", "setpc", "b64"] [Osgpr [0, 1]])],
              bbEntries = [3, 8],
              bbExit = BbExitDynamic (SgprPair (0, 1))
            },
          BasicBlock -- 7
            { bbInstructions =
                [ (52, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (56, Instruction ["s", "cmp", "eq", "u32"] [Osgpr [1], OConst 0]),
                  (60, Instruction ["s", "cbranch", "scc1"] [OConst 3])
                ],
              bbEntries = [0, 2],
              bbExit = BbExitCondJump 10 8
            },
          BasicBlock -- 8
            { bbInstructions = [(64, Instruction ["s", "call", "b64"] [Osgpr [0, 1], OConst 65531])],
              bbEntries = [7],
              bbExit = BbExitJumpSavePc (SgprPair (0, 1)) 6
            },
          BasicBlock -- 9
            { bbInstructions =
                [ (68, Instruction ["v", "mov", "b32", "e32"] [Ovgpr [0], Osgpr [1]]),
                  (72, Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 1.0, Ovgpr [0]])
                ],
              bbEntries = [6],
              bbExit = BbExitFallThrough 10
            },
          BasicBlock -- 10
            { bbInstructions = [(76, Instruction ["s", "setpc", "b64"] [Osgpr [10, 11]])],
              bbEntries = [7, 9],
              bbExit = BbExitDynamic (SgprPair (10, 11))
            },
          BasicBlock -- 11
            { bbInstructions = [(80, Instruction ["s", "endpgm"] [])],
              bbEntries = [5],
              bbExit = BbExitTerminal
            }
        ]

  it "handles function calls with loops" $ do
    (cfg, _) <-
      loadFirstKernel . gfx908Kernel "cfg_s_call_with_loop" $
        [i|
          bb0:
          s_call_b64 s[10:11], fun1
          s_endpgm

          fun1:
          s_waitcnt lgkmcnt(0)
          s_add_u32 s1, s1, -1
          s_cmp_eq_u32 s1, 0
          s_cbranch_scc1 fun1_end
          s_branch fun1
          fun1_end:
          s_setpc_b64 s[10:11]
        |]
    cfg
      `shouldBe` CFG
        [ BasicBlock -- 0
            { bbInstructions = [(0, Instruction ["s", "call", "b64"] [Osgpr [10, 11], OConst 1])],
              bbEntries = [],
              bbExit = BbExitJumpSavePc (SgprPair (10, 11)) 2
            },
          BasicBlock -- 1
            { bbInstructions = [(4, Instruction ["s", "endpgm"] [])],
              bbEntries = [4],
              bbExit = BbExitTerminal
            },
          BasicBlock -- 2
            { bbInstructions =
                [ (8, Instruction ["s", "waitcnt"] [Olgkmcnt 0]),
                  (12, Instruction ["s", "add", "u32"] [Osgpr [1], Osgpr [1], OConst (-1)]),
                  (16, Instruction ["s", "cmp", "eq", "u32"] [Osgpr [1], OConst 0]),
                  (20, Instruction ["s", "cbranch", "scc1"] [OConst 1])
                ],
              bbEntries = [0, 3],
              bbExit = BbExitCondJump 4 3
            },
          BasicBlock -- 3
            { bbInstructions = [(24, Instruction ["s", "branch"] [OConst 65531])],
              bbEntries = [2],
              bbExit = BbExitJump 2
            },
          BasicBlock -- 4
            { bbInstructions = [(28, Instruction ["s", "setpc", "b64"] [Osgpr [10, 11]])],
              bbEntries = [2],
              bbExit = BbExitDynamic (SgprPair (10, 11))
            }
        ]
