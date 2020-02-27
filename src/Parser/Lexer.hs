module Parser.Lexer where

import           Parser.Types                   ( Parser(..) )

import           Control.Monad
import qualified Data.Char                     as Char
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

identToken :: Parser String
identToken = lookAhead letterChar *> takeWhile1P (Just "ident") snakeIdent
  where snakeIdent c = Char.isAlphaNum c || c == '_'

signedInt :: Parser Int
signedInt = lexeme $ L.signed space L.decimal

-- A word lexeme does _not_ consume the suceeding newline tokens
wordLexeme :: Parser a -> Parser a
wordLexeme = L.lexeme whitespaceSingleLine

symbol :: String -> Parser String
symbol = L.symbol whitespace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

untilSpace :: String -> Parser String
untilSpace token = takeWhile1P (Just token) (not . Char.isSpace)

whitespace :: Parser ()
whitespace = L.space space1 lineComment empty

whitespaceSingleLine :: Parser ()
whitespaceSingleLine = L.space tabSpaces1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

tabSpaces1 :: Parser ()
tabSpaces1 = void $ takeWhile1P (Just "tab or space") tabOrSpace
  where tabOrSpace c = c == ' ' || c == '\t'
