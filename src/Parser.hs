{-# LANGUAGE GADTs #-}

module Parser
  ( parseInstruction,
    Instruction (..),
    Operand (..),
  )
where

import qualified Data.Char as Char
import Data.List (delete)

data Instruction = Instruction String [Operand]
  deriving (Eq, Show, Read)

data Operand
  = Osgpr [Int]
  | Ovgpr [Int]
  | Ottmp [Int]
  | OConst Int
  | OOther String
  deriving (Eq, Show, Read)

parseInstruction :: String -> Instruction
parseInstruction input = Instruction opcode operands
  where
    (opcode, operands) = case break (== ' ') input of
      (opcode, "") -> (opcode, [])
      (opcode, ' ' : ops) -> (opcode, reverse $ collectOperands ops [])
    collectOperands [] acc = acc
    collectOperands (',' : ' ' : rest) acc = collectOperands rest acc
    collectOperands opstr acc =
      let (op, rest) = break (== ',') opstr
       in collectOperands rest (parseOperand op : acc)

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
