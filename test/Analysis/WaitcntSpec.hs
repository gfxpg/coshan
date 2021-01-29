{-# LANGUAGE QuasiQuotes #-}

module Analysis.WaitcntSpec where

import Analysis.Waitcnt
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import Data.String.Interpolate (i)
import Helpers
import Test.Hspec

spec :: Spec
spec = describe "memory requests dependency resolution using s_waitcnt" $ do
  it "checks vector memory reads with buffer instructions" $ do
    (cfg, kernel) <-
      loadGfx900Kernel
        [i|waitcnt_buffer_load|]
        [i|
        buffer_load_dwordx4 v[0:3], off, s[0:3], 0
        buffer_load_dwordx4 v[4:7], off, s[0:3], 0 offset:16
        v_add_f32 v0, v1, 1.0
      |]
    -- TODO: location info
    checkWaitcnts kernel cfg `shouldBe` ["vmcnt(1)"]
