{-# LANGUAGE QuasiQuotes #-}

module Analysis.WaitcntSpec where

import Analysis.Waitcnt
import Control.Monad (forM_)
import ControlFlow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import Data.String.Interpolate (i)
import Disassembler
import Helpers
import Reporting
import Test.Hspec

spec :: Spec
spec = describe "memory requests dependency resolution using s_waitcnt" $ do
  it "checks vector memory reads with buffer instructions" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        [i|waitcnt_buffer_load|]
        [i|
          buffer_load_dwordx4 v[0:3], off, s[0:3], 0           // PC = 0
          buffer_load_dwordx4 v[4:7], off, s[0:3], 0 offset:16 // PC = 8
          v_add_f32 v0, v2, 1.0                                // PC = 16
          v_mov_b32 v1, 0                                      // PC = 24
          v_add_f32 v1, v1, 1.0                                // PC = 28, shouldn't produce an error because v1 is overwritten
        |]
    checkWaitcnts kernel cfg
      `shouldBe` [ LogMessage
                     16
                     [ LogText "Missing",
                       LogInstruction "s_waitcnt vmcnt(1)",
                       LogText "before accessing register",
                       LogOperand (Ovgpr [2]),
                       LogText "read from memory at",
                       LogInstructionPath [0]
                     ]
                 ]
  it "recognizes s_waitcnt vmcnt(N)" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        [i|waitcnt_buffer_load|]
        [i|
        buffer_load_dwordx4 v[0:3], off, s[0:3], 0           // PC = 0
        buffer_load_dwordx4 v[4:7], off, s[0:3], 0 offset:16 // PC = 8
        s_waitcnt vmcnt(1)                                   // PC = 16
        v_add_f32 v0, v2, 1.0                                // PC = 20
        v_add_f32 v0, v0, v4                                 // PC = 28
      |]
    checkWaitcnts kernel cfg
      `shouldBe` [ LogMessage
                     28
                     [ LogText "Missing",
                       LogInstruction "s_waitcnt vmcnt(0)",
                       LogText "before accessing register",
                       LogOperand (Ovgpr [4]),
                       LogText "read from memory at",
                       LogInstructionPath [8]
                     ]
                 ]
  it "recognizes s_waitcnt 0" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        [i|waitcnt_buffer_load|]
        [i|
        buffer_load_dwordx4 v[0:3], off, s[0:3], 0           // PC = 0
        buffer_load_dwordx4 v[4:7], off, s[0:3], 0 offset:16 // PC = 8
        s_waitcnt 0                                          // PC = 16
        v_add_f32 v0, v2, 1.0                                // PC = 20
        v_add_f32 v0, v0, v4                                 // PC = 28
      |]
    checkWaitcnts kernel cfg `shouldBe` []
  it "recognizes dependencies in loops" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        [i|waitcnt_loop|]
        [i|
          buffer_load_dword v0, off, s[0:3], 0 // PC = 0
          s_nop 0
          s_nop 0
          bb16:
            v_add_f32_e32 v0, 1.0, v0          // PC = 16
            v_cmp_lt_f32_e32 vcc, 1.0, v0      // PC = 20
            s_and_b64 vcc, exec, vcc           // PC = 24
            s_cbranch_vccnz bb16               // PC = 28
          bb32:
            buffer_load_dwordx2 v[0:1], off, s[0:3], 4
            buffer_load_dwordx2 v[4:5], off, s[0:3], 12
          bb48:
            v_mov_b32 v2, 1.0                  // PC = 48
            v_add_f32_e32 v2, 1.0, v2          // PC = 52
            v_cmp_nlt_f32_e32 vcc, 1.0, v1     // PC = 56
            v_add_f32_e32 v1, 0x40a00000, v2   // PC = 60
            s_and_b64 vcc, exec, vcc           // PC = 68
            s_cbranch_vccnz bb16               // PC = 72
          bb76:
            v_mov_b32 v2, 0.0                  // PC = 76
            s_cbranch_vccnz bb32               // PC = 80
            s_branch bb48                      // PC = 84
      |]
    checkWaitcnts kernel cfg
      `shouldBe` [ LogMessage 16 [LogText "Missing", LogInstruction "s_waitcnt vmcnt(0)", LogText "before accessing register", LogOperand (Ovgpr [0]), LogText "read from memory at", LogInstructionPath [0]],
                   LogMessage 16 [LogText "Missing", LogInstruction "s_waitcnt vmcnt(1)", LogText "before accessing register", LogOperand (Ovgpr [0]), LogText "read from memory at", LogInstructionPath [32]],
                   LogMessage 56 [LogText "Missing", LogInstruction "s_waitcnt vmcnt(1)", LogText "before accessing register", LogOperand (Ovgpr [1]), LogText "read from memory at", LogInstructionPath [32]]
                 ]