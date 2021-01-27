{-# LANGUAGE QuasiQuotes #-}

module Analysis.HazardRWLaneSpec where

import Analysis.HazardRWLane
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import Data.String.Interpolate (i)
import Helpers
import Test.Hspec

spec :: Spec
spec = describe "v_{read,write}lane with sgpr selector modified by valu op hazard detection" $ do
  it "recognizes s_nop with insufficient wait states" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        "hazard_readlane_s_nop"
        [i|
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 0: s[2:3] <- carry bits
          s_nop 2                         // PC = 8: 3 wait states
          v_readlane_b32 s1, v0, s3       // PC = 12: hazard: s3 has been modified by a VALU op, requires 4 wait states
        |]
    checkRwLaneHazards kernel cfg
      `shouldBe` [ LogMessage
                     12
                     [ LogText "Missing 1 wait state for v_readlane_b32 with an SGPR lane selector modified by a VALU instruction:",
                       LogInstructionPath [0, 8]
                     ]
                 ]
  it "follows conditional branches" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        "hazard_readlane_cond_br"
        [i|
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 0: s[2:3] <- carry bits
          s_nop 0                         // PC = 8: 1 wait state
          s_cbranch_scc0 readlane         // PC = 12
          s_nop 0                         // PC = 16
          s_nop 0                         // PC = 20
          s_nop 0                         // PC = 24
          readlane:
          v_readlane_b32 s1, v0, s3       // PC = 28: hazard: s3 has been modified by a VALU op, requires 4 wait states
        |]
    checkRwLaneHazards kernel cfg
      `shouldBe` [ LogMessage
                     28
                     [ LogText "Missing 2 wait states for v_readlane_b32 with an SGPR lane selector modified by a VALU instruction:",
                       LogInstructionPath [0, 8, 12]
                     ]
                 ]
  it "handles loops with hazards" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        "hazard_readlane_loop"
        [i|
          prelude:
          s_mov_b32 s3, 0                 // PC = 0
          s_branch next                   // PC = 4
          loop:
          v_readlane_b32 s1, v0, s3       // PC = 8: hazard: s3 has been modified by a VALU op, requires 4 wait states
          s_nop 4                         // PC = 16
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 20: s[2:3] <- carry bits
          next:
          s_cbranch_scc0 loop             // PC = 28
          s_endpgm
        |]
    checkRwLaneHazards kernel cfg
      `shouldBe` [ LogMessage
                     8
                     [ LogText "Missing 3 wait states for v_readlane_b32 with an SGPR lane selector modified by a VALU instruction:",
                       LogInstructionPath [20, 28]
                     ]
                 ]
  it "handles loops without hazards" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        "hazard_readlane_loop"
        [i|
          prelude:
          s_mov_b32 s3, 0                 // PC = 0
          s_branch next                   // PC = 4
          loop:
          s_nop 4                         // PC = 16
          v_readlane_b32 s1, v0, s3       // PC = 8: hazard: s3 has been modified by a VALU op, requires 4 wait states
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 20: s[2:3] <- carry bits
          next:
          s_cbranch_scc0 loop             // PC = 28
          s_endpgm
        |]
    checkRwLaneHazards kernel cfg `shouldBe` []
