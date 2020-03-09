module Parser.GeneralSpec where

import           Test.Hspec
import           Helper                         ( runParserCase )


spec :: Spec
spec = describe "parser" $ do
  it "handles a simple case" $ runParserCase "simple"
