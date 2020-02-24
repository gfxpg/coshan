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
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

asmlst :: Parser Listing
asmlst = lst <* many newline <* eof

lst :: Parser Listing
lst = do
    directives <- many directive <* takeRest
    pure (Listing directives [])

directive :: Parser Directive
directive = do
    key <- many spaceOrTab *> directiveKey
    val <- option "" directiveValue
    newline
    pure $ Directive key val
  where
    directiveKey   = lexeme (char '.' *> untilSpace ".directive")
    directiveValue = lexeme (untilSpace "directive value")
    untilSpace token = takeWhile1P (Just token) (not . Char.isSpace)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace
  where
    whitespace  = L.space (void $ spaceOrTab) lineComment empty
    lineComment = L.skipLineComment ";"

spaceOrTab :: Parser Char
spaceOrTab = oneOf " \t"
