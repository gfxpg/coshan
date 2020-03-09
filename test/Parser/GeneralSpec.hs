module Parser.GeneralSpec where

import           Test.Hspec
import           Helper                         ( runParserCase )


spec :: Spec
spec = describe "parser" $ do
  it "handles a simple if construct" $ runParserCase "simple-if"
  it "handles a simple for construct" $ runParserCase "simple-for"
