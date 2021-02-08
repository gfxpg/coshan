{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Disassembler.ElfReaderSpec where

import Coshan.Disassembler
import qualified Data.ByteString as BStr
import Helpers
import Test.Hspec

spec :: Spec
spec = describe "elf reader" $ do
  it "extracts all kernels from code object V3" $ do
    elf <-
      compileCo $ CodeObject {coCpu = "gfx900", coMetadataV3 = True, coKernels = [("test_kernel_a", "v_nop\ns_endpgm"), ("test_kernel_b", "s_endpgm")]}
    kernels <- readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    length kernels `shouldBe` 2
    disasmKernelName (head kernels) `shouldBe` "test_kernel_a"
    disasmKernelName (kernels !! 1) `shouldBe` "test_kernel_b"
    let paddingInstructions = (,"s_nop 0") <$> [8, 12 .. 252] -- kernels are aligned on a 256-byte boundary (?)
    disasmInstructions (head kernels) `shouldBe` [(0, "v_nop"), (4, "s_endpgm")] ++ paddingInstructions
    disasmInstructions (kernels !! 1) `shouldBe` [(0, "s_endpgm")]

  it "extracts all kernels from code object V2" $ do
    elf <- BStr.readFile "test/Disassembler/Cases/kernels.hsaco"
    kernels <- readElf (DisasmTarget {disasmTriple = "amdgcn--amdhsa", disasmCPU = "gfx900"}) elf
    length kernels `shouldBe` 2
    disasmKernelName (head kernels) `shouldBe` "_Z10cond_writePfPKff"
    disasmInstructions (head kernels)
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
    disasmKernelName (kernels !! 1) `shouldBe` "_Z15simple_for_loopPfm"
    disasmInstructions (kernels !! 1)
      `shouldBe` [ (0, "s_load_dwordx4 s[0:3], s[4:5], 0x0"),
                   (8, "s_waitcnt lgkmcnt(0)"),
                   (12, "s_cmp_eq_u64 s[2:3], 0"),
                   (16, "s_cbranch_scc1 16"),
                   (20, "v_mov_b32_e32 v0, 0x41100000"),
                   (28, "s_load_dword s4, s[0:1], 0x0"),
                   (36, "v_mov_b32_e32 v2, s1"),
                   (40, "v_mov_b32_e32 v1, s0"),
                   (44, "s_add_u32 s0, s0, 4"),
                   (48, "s_addc_u32 s1, s1, 0"),
                   (52, "s_add_u32 s2, s2, -1"),
                   (56, "s_addc_u32 s3, s3, -1"),
                   (60, "s_cmp_lg_u64 s[2:3], 0"),
                   (64, "s_waitcnt lgkmcnt(0)"),
                   (68, "v_add_f32_e32 v3, s4, v0"),
                   (72, "global_store_dword v[1:2], v3, off"),
                   (80, "s_cbranch_scc1 65522"),
                   (84, "s_endpgm")
                 ]
