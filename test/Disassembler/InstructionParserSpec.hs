module Disassembler.InstructionParserSpec where

import Coshan.Disassembler (Instruction (..), Operand (..), parseInstruction)
import Test.Hspec

spec :: Spec
spec = describe "parser" $ do
  it "parses instructions with no operands" $
    parseInstruction "s_endpgm"
      `shouldBe` Instruction "s_endpgm" []

  it "parses sgpr operands" $
    parseInstruction "s_load_dwordx4 s[0:3], s[0:1], 0x00"
      `shouldBe` Instruction "s_load_dwordx4" [Osgpr [0, 1, 2, 3], Osgpr [0, 1], OConst 0]

  it "parses vgpr operands" $
    parseInstruction "global_store_dword v[1:2], v3, off"
      `shouldBe` Instruction "global_store_dword" [Ovgpr [1, 2], Ovgpr [3], OOther "off"]

  it "parses ttmp operands" $
    parseInstruction "buffer_store_dword v1, off, ttmp[8:11], ttmp5, 0"
      `shouldBe` Instruction "buffer_store_dword" [Ovgpr [1], OOther "off", Ottmp [8, 9, 10, 11], Ottmp [5], OConst 0]

  it "parses constant hexadecimal numbers" $
    parseInstruction "v_mov_b32_e32 v0, 0x41100000"
      `shouldBe` Instruction "v_mov_b32_e32" [Ovgpr [0], OConst 1091567616]

  it "parses constant decimal numbers" $
    parseInstruction "s_cbranch_scc1 65522"
      `shouldBe` Instruction "s_cbranch_scc1" [OConst 65522]

  it "parses constant negative numbers" $
    parseInstruction "s_add_u32 s2, s2, -1"
      `shouldBe` Instruction "s_add_u32" [Osgpr [2], Osgpr [2], OConst (-1)]

  it "parses vcc/exec operands" $ do
    parseInstruction "v_cmp_eq_u32_e32 vcc, 3, v2"
      `shouldBe` Instruction "v_cmp_eq_u32_e32" [OOther "vcc", OConst 3, Ovgpr [2]]
    parseInstruction "s_mov_b32 exec_lo, s1"
      `shouldBe` Instruction "s_mov_b32" [OOther "exec_lo", Osgpr [1]]

  it "parses s_waitcnt operands" $ do
    parseInstruction "s_waitcnt lgkmcnt(0) vmcnt(0) vscnt(0)"
      `shouldBe` Instruction "s_waitcnt" [OOther "lgkmcnt(0)", OOther "vmcnt(0)", OOther "vscnt(0)"]
    parseInstruction "s_waitcnt lgkmcnt(0) & vmcnt(0) & vscnt(0)"
      `shouldBe` Instruction "s_waitcnt" [OOther "lgkmcnt(0)", OOther "vmcnt(0)", OOther "vscnt(0)"]

  it "parses ds instruction operands" $
    parseInstruction "ds_read2_b32 v[1:2], v0 offset0:0 offset1:4"
      `shouldBe` Instruction "ds_read2_b32" [Ovgpr [1, 2], Ovgpr [0], OOther "offset0:0", OOther "offset1:4"]
