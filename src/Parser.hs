module Parser
    ( asmlst
    , module Parser.Types
    )
where

import           Parser.Types
import           Text.Megaparsec
import           Control.Applicative
import           Control.Monad
import           Data.Void
import           Text.Megaparsec.Char

type Parser = Parsec Void String

asmlst :: Parser Listing
asmlst = lst <* eof

lst :: Parser Listing
lst = newline *> pure (Listing [])
