module Format.Cfg where

import Coshan.ControlFlow
import Coshan.Disassembler
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import Data.Semigroup (stimes)
import System.IO (stdout)

printCfg :: DisassembledKernel -> CFG -> IO ()
printCfg kernel = B.hPutBuilder stdout . formatCfg kernel

formatCfg :: DisassembledKernel -> CFG -> B.Builder
formatCfg kernel (CFG bbs) = mconcat $ putBb <$> zip [0 ..] bbs
  where
    putBb (bbIdx, BasicBlock {bbInstructions = insts, bbEntries = entryIdxs, bbExit = exit}) =
      putBbLabel bbIdx
        <> ":\t /* predecessors: "
        <> putBbEntries entryIdxs
        <> ", exit: "
        <> putBbExit exit
        <> " */\n"
        <> putInstructions insts ""
        <> ch '\n'
      where
        putBbLabel i = "bb" <> B.intDec i
        putInstructions ((pc, i) : rest@((nextPc, _) : _)) acc =
          putInstructions rest $ acc <> putInstruction pc i (nextPc - pc) <> "\n"
        putInstructions [(pc, i)] acc
          | bbIdx < length bbs - 1,
            ((nextPc, _) : _) <- bbInstructions (bbs !! (bbIdx + 1)) =
            acc <> putInstruction pc i (nextPc - pc) <> "\n"
          | otherwise =
            acc <> putInstruction pc i (BStr.length (disasmInstructionsBin kernel) - pc)
        putInstructions [] acc = acc
        putBbEntries []
          | ((0, _) : _) <- insts = "none (program start)"
          | otherwise = "none (unreachable)"
        putBbEntries e = strJoin ", " (putBbLabel <$> e)
        putBbExit (BbExitCondJump bb1 bb2) = "conditional jump to bb" <> B.intDec bb1 <> " or bb" <> B.intDec bb2
        putBbExit (BbExitJump bb1) = "jump to bb" <> B.intDec bb1
        putBbExit (BbExitFallThrough bb1) = "bb" <> B.intDec bb1
        putBbExit (BbExitJumpSavePc _ bb1) = "call to bb" <> B.intDec bb1
        putBbExit (BbExitDynamic _) = "dynamic return (jump to SGPR)"
        putBbExit BbExitTerminal = "none (program end)"
    putInstruction pc (Instruction op ops) size = B.lazyByteString asmInstruction <> putMeta
      where
        asmInstruction = B.toLazyByteString (opcode <> ch ' ' <> operands)
          where
            opcode = strJoin (ch '_') $ B.byteString <$> op
            operands = strJoin ", " $ putOperand <$> ops
        putMeta = stimes spacesAfterAsm (ch ' ') <> " // " <> B.int64HexFixed (fromIntegral pc) <> ":" <> binDwords binInstruction ""
          where
            binInstruction = BStr.take size . BStr.drop pc $ disasmInstructionsBin kernel
            spacesAfterAsm = max 1 (100 - L.length asmInstruction)
            binDwords inst acc
              | BStr.null inst = acc
              | otherwise =
                let (dword, rest) = BStr.splitAt 4 inst
                 in binDwords rest $ acc <> " " <> B.byteStringHex (BStr.reverse dword)
        putOperand o = case o of
          Osgpr rs -> ch 's' <> putRegs rs
          Ovgpr rs -> ch 'v' <> putRegs rs
          Ottmp rs -> "ttmp" <> putRegs rs
          Ovmcnt ctr -> "vmcnt(" <> B.intDec ctr <> ch ')'
          Olgkmcnt ctr -> "lgkmcnt(" <> B.intDec ctr <> ch ')'
          Oexpcnt ctr -> "expcnt(" <> B.intDec ctr <> ch ')'
          OConst int -> B.intDec int
          OConstF f -> B.floatDec f
          OCtrl ctrl -> B.byteString ctrl
        putRegs [i] = B.intDec i
        putRegs is
          | from <- head is,
            to <- last is,
            from == minimum is,
            to == maximum is =
            ch '[' <> B.intDec from <> ch ':' <> B.intDec to <> ch ']'
          | otherwise = error $ "Cannot print register range " ++ show is
    strJoin sep = mconcat . intersperse sep
    ch = B.char8
