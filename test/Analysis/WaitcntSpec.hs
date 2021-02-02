{-# LANGUAGE QuasiQuotes #-}

module Analysis.WaitcntSpec where

import Analysis.Waitcnt
import Control.Monad (forM_)
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
