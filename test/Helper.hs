module Helper where

import           Test.Hspec                     ( Expectation(..) )
import           Text.Megaparsec                ( Parsec(..)
                                                , ShowErrorComponent(..)
                                                , parse
                                                )
import           Test.Hspec.Megaparsec

runParserCase
  :: (Read s, Show s, Eq s)
  => ShowErrorComponent e => Parsec e String s -> String -> Expectation
runParserCase parser casefile = do
  input  <- readFile inputPath
  output <- read <$> readFile outputPath
  parse parser casefile input `shouldParse` output
 where
  outputPath = "test/Parser/Cases/" ++ casefile
  inputPath  = outputPath ++ ".isa"
