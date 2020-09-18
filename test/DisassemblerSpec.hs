{-# LANGUAGE OverloadedStrings #-}

module DisassemblerSpec where

import qualified Data.ByteString as BStr
import Disassembler
import Test.Hspec

spec :: Spec
spec = describe "disassembler" $ do
  it "extracts assembly code from an ELF binary" $ do
    elf <- BStr.readFile "test/Disassembler/Cases/dump-gfx900.hsaco"
    disasm <- readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    disasmInstructions disasm
      `shouldBe` [ (0, "s_load_dwordx4 s[0:3], s[4:5], 0x0"),
                   (8, "s_load_dword s4, s[4:5], 0x10"),
                   (16, "s_waitcnt lgkmcnt(0)"),
                   (20, "s_load_dword s2, s[2:3], 0x0"),
                   (28, "v_mov_b32_e32 v0, s4"),
                   (32, "s_waitcnt lgkmcnt(0)"),
                   (36, "v_cmp_neq_f32_e32 vcc, s2, v0"),
                   (40, "s_and_b64 vcc, exec, vcc"),
                   (44, "s_cbranch_vccnz 6"),
                   (48, "v_mov_b32_e32 v0, s0"),
                   (52, "v_mov_b32_e32 v1, s1"),
                   (56, "v_mov_b32_e32 v2, 0x44ccc000"),
                   (64, "global_store_dword v[0:1], v2, off"),
                   (72, "s_endpgm")
                 ]
