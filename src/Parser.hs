{-# LANGUAGE GADTs #-}

module Parser
    ( asmlst
    , module Parser.Types
    )
where

import           Parser.Types
import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import           Data.Void
import qualified Data.Char                     as Char
import qualified Data.Map.Strict               as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

asmlst :: Parser Listing
asmlst = lst <* eof

lst :: Parser Listing
lst = do
    space
    skipMany directive
    asmLabel -- skip entry label
    kcode <- kernelCodeT
    takeRest
    pure (Listing kcode [])

directive :: Parser (String, String)
directive = (,) <$> directiveKey <*> option "" directiveValue
  where
    directiveKey   = lexeme (char '.' *> untilSpace ".directive" <* space)
    directiveValue = lexeme (untilSpace "directive value")
    untilSpace token = takeWhile1P (Just token) (not . Char.isSpace)

kernelCodeT :: Parser KernelCodeT
kernelCodeT = do
    symbol ".amd_kernel_code_t"
    directives <- some kvPair
    symbol ".end_amd_kernel_code_t"
    pure $ KernelCodeT $ Map.fromList directives
    where kvPair = (,) <$> (ident <* lexeme (char '=')) <*> signedInt

asmLabel :: Parser String
asmLabel = lexeme (takeWhile1P (Just "label") (/= ':') <* char ':')

--

ident :: Parser String
ident = lexeme (lookAhead letterChar *> takeWhile1P (Just "ident") snakeIdent)
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

whitespace :: Parser ()
whitespace = L.space space1 lineComment empty

whitespaceSingleLine :: Parser ()
whitespaceSingleLine = L.space tabSpaces1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

tabSpaces1 :: Parser ()
tabSpaces1 = void $ takeWhile1P (Just "tab or space") tabOrSpace
    where tabOrSpace c = c == ' ' || c == '\t'
