{-# LANGUAGE OverloadedStrings #-}

module Coshan.Disassembler.InstructionParser (parseInstruction) where

import Control.Monad ((>=>))
import Coshan.Disassembler.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Char as Char
import Data.List (delete)

parseInstruction :: ByteString -> Instruction
parseInstruction input = Instruction (BC8.unpack opcode) operands
  where
    (opcode, opStr) = BC8.break (== ' ') input
    operands = parseOperand <$> filter (not . BC8.null) (BC8.splitWith (\c -> c == ' ' || c == ',') opStr)

parseOperand :: ByteString -> Operand
parseOperand str = case prefix of
  's' | prefixRest == '[' || Char.isDigit prefixRest -> parseRegisterOperand Osgpr rest
  'v' | prefixRest == '[' || Char.isDigit prefixRest -> parseRegisterOperand Ovgpr rest
  't' | Just tmp <- BC8.stripPrefix "tmp" rest -> parseRegisterOperand Ottmp tmp
  '0' | Just hex <- BC8.stripPrefix "x" rest -> OConst $ parseNumber 16 hex
  '-' -> OConst $ (-1) * parseNumber 10 rest
  'v' | Just (c, _) <- (BC8.stripPrefix "mcnt(" >=> BC8.readInt) rest -> Ovmcnt c
  'v' | Just (c, _) <- (BC8.stripPrefix "scnt(" >=> BC8.readInt) rest -> Ovscnt c
  'l' | Just (c, _) <- (BC8.stripPrefix "gkmcnt(" >=> BC8.readInt) rest -> Olgkmcnt c
  'e' | Just (c, _) <- (BC8.stripPrefix "xpcnt(" >=> BC8.readInt) rest -> Oexpcnt c
  _ | BC8.all Char.isDigit str -> OConst $ parseNumber 10 str
  _ -> OOther $ BC8.unpack str
  where
    Just (prefix, rest) = BC8.uncons str
    prefixRest = BC8.head rest

parseRegisterOperand :: ([Int] -> Operand) -> ByteString -> Operand
parseRegisterOperand ctr input = case BC8.break (== ':') input of
  (idx, rest)
    | BC8.null rest ->
      ctr [parseNumber 10 idx]
  (start, rest)
    | Just from <- BC8.stripPrefix "[" start,
      Just to <- (BC8.stripPrefix ":" >=> BC8.stripSuffix "]") rest ->
      ctr $ enumFromTo (parseNumber 10 from) (parseNumber 10 to)
  _ -> error $ "Unable to parse register operand string \"" ++ BC8.unpack input ++ "\""

parseNumber :: Int -> ByteString -> Int
parseNumber base = BC8.foldl' (\acc c -> acc * base + Char.digitToInt c) 0
