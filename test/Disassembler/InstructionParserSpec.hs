{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Disassembler.InstructionParserSpec where

import Coshan.Disassembler
import Data.String.Interpolate (i)
import Helpers
import Test.Hspec

spec :: Spec
spec = describe "parseInstruction" $ do
  it "extracts registers" $ do
    elf <-
      compileCo . gfx900Kernel "disasm_regs" $
        [i|
          s_load_dword s3, s[0:1], 0x00
          global_store_dword v[1:2], v3, off
          buffer_store_dword v1, off, ttmp[8:11], ttmp5 offset:0
        |]
    kernel <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    parseInstruction . snd <$> disasmInstructions kernel
      `shouldBe` [ Instruction ["s", "load", "dword"] [Osgpr [3], Osgpr [0, 1], OConst 0],
                   Instruction ["global", "store", "dword"] [Ovgpr [1, 2], Ovgpr [3], OCtrl "off"],
                   Instruction ["buffer", "store", "dword"] [Ovgpr [1], OCtrl "off", Ottmp [8, 9, 10, 11], Ottmp [5]]
                 ]

  it "extracts integer literals" $ do
    elf <-
      compileCo . gfx900Kernel "disasm_int_lits" $
        [i|
          v_mov_b32_e32 v0, 0x41100000
          s_cbranch_scc1 65522
          s_add_u32 s2, s2, -1
        |]
    kernel <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    parseInstruction . snd <$> disasmInstructions kernel
      `shouldBe` [ Instruction ["v", "mov", "b32", "e32"] [Ovgpr [0], OConst 1091567616],
                   Instruction ["s", "cbranch", "scc1"] [OConst 65522],
                   Instruction ["s", "add", "u32"] [Osgpr [2], Osgpr [2], OConst (-1)]
                 ]

  it "extracts float literals" $ do
    elf <-
      compileCo . gfx900Kernel "disasm_float_lits" $
        [i|
          v_add_f32_e32 v0, 1.0, v0
          v_add_f32_e32 v0, -1.0, v0
          v_add_f32_e32 v0, 0.5, v0
          v_add_f32_e32 v0, -0.5, v0
          v_add_f32_e32 v0, 0.15915494, v0
        |]
    kernel <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    parseInstruction . snd <$> disasmInstructions kernel
      `shouldBe` [ Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 1.0, Ovgpr [0]],
                   Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF (-1.0), Ovgpr [0]],
                   Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 0.5, Ovgpr [0]],
                   Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF (-0.5), Ovgpr [0]],
                   Instruction ["v", "add", "f32", "e32"] [Ovgpr [0], OConstF 0.15915494, Ovgpr [0]]
                 ]

  it "extracts control operands" $ do
    elf <-
      compileCo . gfx900Kernel "disasm_ctrl" $
        [i|
          ds_read2_b32 v[1:2], v0 offset0:0 offset1:4
          v_cmp_eq_u32_e32 vcc, 3, v2
          s_mov_b32 exec_lo, s1
        |]
    kernel <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    parseInstruction . snd <$> disasmInstructions kernel
      `shouldBe` [ Instruction ["ds", "read2", "b32"] [Ovgpr [1, 2], Ovgpr [0], OCtrl "offset1:4"],
                   Instruction ["v", "cmp", "eq", "u32", "e32"] [OCtrl "vcc", OConst 3, Ovgpr [2]],
                   Instruction ["s", "mov", "b32"] [OCtrl "exec_lo", Osgpr [1]]
                 ]

  it "extracts s_waitcnt operands" $ do
    elf <-
      compileCo . gfx900Kernel "disasm_waitcnt" $
        [i|
          s_waitcnt 0
          s_waitcnt lgkmcnt(7)
          s_waitcnt lgkmcnt(10) vmcnt(4) expcnt(0)
        |]
    kernel <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    parseInstruction . snd <$> disasmInstructions kernel
      `shouldBe` [ Instruction ["s", "waitcnt"] [Ovmcnt 0, Oexpcnt 0, Olgkmcnt 0],
                   Instruction ["s", "waitcnt"] [Olgkmcnt 7],
                   Instruction ["s", "waitcnt"] [Ovmcnt 4, Oexpcnt 0, Olgkmcnt 10]
                 ]

  it "handles instructions with no operands" $ do
    elf <-
      compileCo . gfx900Kernel "disasm_zero_op" $
        [i|
          s_endpgm
          v_nop
        |]
    kernel <- head <$> readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    parseInstruction . snd <$> disasmInstructions kernel
      `shouldBe` [ Instruction ["s", "endpgm"] [],
                   Instruction ["v", "nop"] []
                 ]
