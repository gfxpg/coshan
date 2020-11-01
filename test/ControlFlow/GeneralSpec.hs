module ControlFlow.GeneralSpec where

import           Test.Hspec

spec :: Spec
spec = describe "cfg construction" $ do
  it "does nothing" $ 0 `shouldBe` 0
  -- it "handles an if condition" $ runControlFlowCase "simple-if"
  -- it "handles a for loop" $ runControlFlowCase "simple-for"
