{-# LANGUAGE GADTs #-}

module Coshan.Disassembler.InstructionParser (parseInstruction) where

import Coshan.Disassembler.Types
import qualified Data.Char as Char
import Data.List (delete)
import Data.List.Split (dropBlanks, dropDelims, oneOf, split)

parseInstruction :: String -> Instruction
parseInstruction input = Instruction opcode operands
  where
    (opcode, opStr) = break (== ' ') input
    operands = parseOperand <$> split (dropBlanks $ dropDelims $ oneOf " ,&") opStr

parseOperand :: String -> Operand
parseOperand ('s' : regs@(i : _)) | i == '[' || Char.isDigit i = parseRegisterOperand Osgpr regs
parseOperand ('v' : regs@(i : _)) | i == '[' || Char.isDigit i = parseRegisterOperand Ovgpr regs
parseOperand ('t' : 't' : 'm' : 'p' : regs@(i : _)) | i == '[' || Char.isDigit i = parseRegisterOperand Ottmp regs
parseOperand ('0' : 'x' : hex) = OConst $ parseNumber 16 hex
parseOperand ('-' : dec) = OConst $ (-1) * parseNumber 10 dec
parseOperand other
  | all Char.isDigit other = OConst $ parseNumber 10 other
  | otherwise = OOther other

parseRegisterOperand :: ([Int] -> Operand) -> String -> Operand
parseRegisterOperand ctr input = case break (== ':') input of
  (idx, "") -> ctr [parseNumber 10 idx]
  ('[' : from, ':' : to) ->
    let fromIdx = parseNumber 10 from
        toIdx = parseNumber 10 $ delete ']' to
     in ctr $ enumFromTo fromIdx toIdx
  _ -> error $ "Unable to parse register operand string \"" ++ input ++ "\""

parseNumber :: Int -> String -> Int
parseNumber base = go 0
  where
    go acc [] = acc
    go acc (c : rest) = go (acc * base + Char.digitToInt c) rest
