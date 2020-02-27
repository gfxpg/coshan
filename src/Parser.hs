{-# LANGUAGE GADTs #-}

module Parser
    ( asmlst
    , module Parser.Types
    )
where

import           Parser.Types
import           Parser.Lexer
import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

asmlst :: Parser Listing
asmlst = do
    -- skip directives and kernel entry label
    space *> skipMany directive *> asmLabel
    Listing <$> kernelCodeT <*> some instruction <* takeRest <* eof

directive :: Parser (String, String)
directive = (,) <$> directiveKey <*> option "" directiveValue
  where
    directiveKey   = lexeme (char '.' *> untilSpace ".directive" <* space)
    directiveValue = lexeme (untilSpace "directive value")

kernelCodeT :: Parser KernelCodeT
kernelCodeT = do
    symbol ".amd_kernel_code_t"
    directives <- some $ (,) <$> key <*> signedInt
    symbol ".end_amd_kernel_code_t"
    pure $ KernelCodeT $ Map.fromList directives
    where key = lexeme identToken <* lexeme (char '=')

asmLabel :: Parser String
asmLabel = lexeme (takeWhile1P (Just "label") (/= ':') <* char ':')

instruction :: Parser Instruction
instruction = lexeme $ do
    ident <- identToken
    choice [label ident, AsmInstr ident <$> operands]
  where
    label name = pure (AsmLabel name) <* char ':'
    operands = many (option ',' (char ',') *> tabSpaces1 *> instructionOperand)

instructionOperand :: Parser Operand
instructionOperand = choice
    [ string "scc" *> pure OpSCC
    , string "vcc" *> pure OpVCC
    , string "exec" *> pure OpExec
    , char 's' *> (OpSGPR <$> regs)
    , char 'v' *> (OpVGPR <$> regs)
    , char '0' *> char 'x' *> (OpConst <$> L.hexadecimal)
    , OpConst <$> L.decimal
    , OpSys <$> takeWhile1P (Just "operand") (\c -> c /= ',' && c /= '\n')
    ]
  where
    regs :: Parser [Int]
    regs = choice [char '[' *> range <* char ']', pure <$> L.decimal]
    range :: Parser [Int]
    range = enumFromTo <$> L.decimal <* char ':' <*> L.decimal
