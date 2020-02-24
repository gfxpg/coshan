module Parser.GeneralSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Text.Megaparsec                ( parse )

import           Helper                         ( runParserCase )
import           Parser                         ( asmlst
                                                , Listing(..)
                                                )

spec :: Spec
spec = describe "parser" $ do
  it "handles a simple case" $ do
    runParserCase asmlst "simple"
