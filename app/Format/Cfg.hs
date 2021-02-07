{-# LANGUAGE OverloadedStrings #-}

module Format.Cfg where

import Coshan.ControlFlow
import Coshan.Disassembler
import qualified Data.ByteString.Builder as B
import Data.List (intersperse)
import System.IO (stdout)

printCfg :: CFG -> IO ()
printCfg = B.hPutBuilder stdout . formatCfg

formatCfg :: CFG -> B.Builder
formatCfg (CFG bbs) = mconcat $ putBb <$> zip [0 ..] bbs
  where
    putBb (bbIdx, BasicBlock {bbInstructions = insts, bbPredecessors = predIdxs, bbSuccessors = succIdxs}) =
      putBbLabel bbIdx
        <> str ":\t /* predecessors: "
        <> strJoin (str ", ") (putBbLabel <$> predIdxs)
        <> str ", successors: "
        <> strJoin (str ", ") (putBbLabel <$> succIdxs)
        <> str " */\n"
        <> strJoin (ch '\n') (putInstruction <$> insts)
        <> ch '\n'
    putBbLabel i = "bb" <> B.intDec i
    putInstruction (pc, Instruction op ops) = putPc <> opcode <> ch ' ' <> operands
      where
        opcode = mconcat $ intersperse (ch '_') $ B.byteString <$> op
        operands = mconcat $ intersperse (str ", ") $ putOperand <$> ops
        putPc = str "/* PC = " <> B.intDec pc <> str "\t*/ "
        putOperand o = case o of
          Osgpr rs -> ch 's' <> putRegs rs
          Ovgpr rs -> ch 'v' <> putRegs rs
          Ottmp rs -> str "ttmp" <> putRegs rs
          Ovmcnt ctr -> str "vmcnt(" <> B.intDec ctr <> ch ')'
          Olgkmcnt ctr -> str "lgkmcnt(" <> B.intDec ctr <> ch ')'
          Oexpcnt ctr -> str "expcnt(" <> B.intDec ctr <> ch ')'
          OConst int -> B.intDec int
          OConstF f -> B.floatDec f
          OCtrl ctrl -> str ctrl
        putRegs [i] = B.intDec i
        putRegs is
          | from <- head is,
            to <- last is,
            from == minimum is,
            to == maximum is =
            ch '[' <> B.intDec from <> ch ':' <> B.intDec to <> ch ']'
          | otherwise = error $ "Cannot print register range " ++ show is
    strJoin sep = mconcat . intersperse sep
    str = B.byteString
    ch = B.char8
