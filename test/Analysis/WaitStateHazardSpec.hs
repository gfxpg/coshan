{-# LANGUAGE QuasiQuotes #-}

module Analysis.WaitStateHazardSpec where

import Control.Monad (forM_)
import Coshan.Analysis.WaitStateHazard
import Coshan.Reporting
import Data.String.Interpolate (i)
import Helpers
import Test.Hspec

spec :: Spec
spec = describe "v_{read,write}lane with sgpr selector modified by valu op hazard detection" $ do
  it "recognizes s_nop with insufficient wait states" $
    forM_ (["v_readlane_b32", "v_writelane_b32"] :: [String]) $ \instruction -> do
      (cfg, kernel) <-
        loadFirstKernel . gfx908Kernel [i|rwlane_s_nop_#{instruction}|] $
          [i|
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 0: s[2:3] <- carry bits
          s_nop 2                         // PC = 8: 3 wait states
          // PC = 12: hazard: s3 has been modified by a VALU op, requires 4 wait states
          #{case instruction of
            "v_readlane_b32" -> "v_readlane_b32 s1, v0, s3" :: String
            "v_writelane_b32" -> "v_writelane_b32 v0, 1, s3" :: String}
        |]
      checkWaitStateHazards kernel cfg
        `shouldBe` [ Error 12 $
                       WaitStatesRequired
                         { wsreqMissingWaitStates = 1,
                           wsreqBacktrace = [8, 0],
                           wsreqExplanation = "A v_readlane/v_writelane instruction with an SGPR lane selector requires 4 wait states after the selector has been modified by a VALU instruction."
                         }
                   ]

  it "follows conditional branches" $
    forM_ (["v_readlane_b32", "v_writelane_b32"] :: [String]) $ \instruction -> do
      (cfg, kernel) <-
        loadFirstKernel . gfx908Kernel [i|rwlane_cond_br_#{instruction}|] $
          [i|
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 0: s[2:3] <- carry bits
          s_nop 0                         // PC = 8: 1 wait state
          s_cbranch_scc0 readlane         // PC = 12
          s_nop 0                         // PC = 16
          s_nop 0                         // PC = 20
          s_nop 0                         // PC = 24
          readlane:
          // PC = 28: hazard: s3 has been modified by a VALU op, requires 4 wait states
          #{case instruction of
            "v_readlane_b32" -> "v_readlane_b32 s1, v0, s3" :: String
            "v_writelane_b32" -> "v_writelane_b32 v0, 1, s3" :: String}
        |]
      checkWaitStateHazards kernel cfg
        `shouldBe` [ Error 28 $
                       WaitStatesRequired
                         { wsreqMissingWaitStates = 2,
                           wsreqBacktrace = [12, 8, 0],
                           wsreqExplanation = "A v_readlane/v_writelane instruction with an SGPR lane selector requires 4 wait states after the selector has been modified by a VALU instruction."
                         }
                   ]

  it "handles loops with hazards" $
    forM_ (["v_readlane_b32", "v_writelane_b32"] :: [String]) $ \instruction -> do
      (cfg, kernel) <-
        loadFirstKernel . gfx908Kernel [i|rwlane_loop_#{instruction}|] $
          [i|
          prelude:
          s_mov_b32 s3, 0                 // PC = 0
          s_branch next                   // PC = 4
          loop:
          // PC = 8: hazard: s3 has been modified by a VALU op, requires 4 wait states
          #{case instruction of
            "v_readlane_b32" -> "v_readlane_b32 s1, v0, s3" :: String
            "v_writelane_b32" -> "v_writelane_b32 v0, 1, s3" :: String}
          s_nop 4                         // PC = 16
          v_add_co_u32 v1, s[2:3], v0, v1 // PC = 20: s[2:3] <- carry bits
          next:
          s_cbranch_scc0 loop             // PC = 28
          s_endpgm
        |]
      checkWaitStateHazards kernel cfg
        `shouldBe` [ Error 8 $
                       WaitStatesRequired
                         { wsreqMissingWaitStates = 3,
                           wsreqBacktrace = [28, 20],
                           wsreqExplanation = "A v_readlane/v_writelane instruction with an SGPR lane selector requires 4 wait states after the selector has been modified by a VALU instruction."
                         }
                   ]

  it "handles loops without hazards" $
    forM_ (["v_readlane_b32", "v_writelane_b32"] :: [String]) $ \instruction -> do
      (cfg, kernel) <-
        loadFirstKernel . gfx908Kernel [i|rwlane_loop_none_#{instruction}|] $
          [i|
          prelude:
          s_mov_b32 s3, 0
          s_branch next
          loop:
          s_nop 4
          #{case instruction of
            "v_readlane_b32" -> "v_readlane_b32 s1, v0, s3" :: String
            "v_writelane_b32" -> "v_writelane_b32 v0, 1, s3" :: String}
          v_add_co_u32 v1, s[2:3], v0, v1
          next:
          s_cbranch_scc0 loop
          s_endpgm
        |]
      checkWaitStateHazards kernel cfg `shouldBe` []
