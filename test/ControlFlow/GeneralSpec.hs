module ControlFlow.GeneralSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Text.Megaparsec                ( parse )

import           Parser                         ( asmlst
                                                , Listing(..)
                                                )
import           ControlFlow                    ( constructGraph )

spec :: Spec
spec = describe "control flow" $ do
  it "constructs a cfg" $ do
    inputFile <- readFile "test/ControlFlow/Cases/simple.isa"
    let Right (Listing _ instructions) = parse asmlst "" inputFile
    let graph                          = constructGraph instructions
    putStrLn $ show graph
