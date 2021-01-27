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
        "hazard_readlane_simple"
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
