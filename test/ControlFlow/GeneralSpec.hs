module ControlFlow.GeneralSpec where

import           Test.Hspec
import           Helper                         ( runControlFlowCase )

spec :: Spec
spec = describe "cfg construction" $ do
  it "handles an if condition" $ runControlFlowCase "simple-if"
  it "handles a for loop" $ runControlFlowCase "simple-for"
