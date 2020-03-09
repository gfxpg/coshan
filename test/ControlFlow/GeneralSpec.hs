module ControlFlow.GeneralSpec where

import           Test.Hspec
import           Helper                         ( runControlFlowCase )

spec :: Spec
spec = describe "control flow" $ do
  it "constructs a simple cfg" $ runControlFlowCase "simple"
