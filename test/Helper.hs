module Helper where

import           Parser                         ( asmlst
                                                , Listing(..)
                                                )
import           ControlFlow                    ( constructGraph )
import           Test.Hspec                     ( Expectation(..)
                                                , shouldBe
                                                )
import           Text.Megaparsec                ( Parsec(..)
                                                , ShowErrorComponent(..)
                                                , parse
                                                )
import           Test.Hspec.Megaparsec

runControlFlowCase :: String -> Expectation
runControlFlowCase casefile = do
  input    <- readFile inputPath
  expected <- read <$> readFile outputPath
  let Right (Listing _ instructions) = parse asmlst casefile input
  constructGraph instructions `shouldBe` expected
 where
  outputPath = "test/ControlFlow/Cases/" ++ casefile
  inputPath  = outputPath ++ ".isa"

runParserCase :: String -> Expectation
runParserCase casefile = do
  input  <- readFile inputPath
  output <- read <$> readFile outputPath
  parse asmlst casefile input `shouldParse` output
 where
  outputPath = "test/Parser/Cases/" ++ casefile
  inputPath  = outputPath ++ ".isa"
