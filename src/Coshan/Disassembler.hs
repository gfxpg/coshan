{-# LANGUAGE NamedFieldPuns #-}

module Coshan.Disassembler
  ( readElf,
    module Coshan.Disassembler.Types,
    module Coshan.Disassembler.InstructionParser,
  )
where

import Coshan.Disassembler.InstructionParser
import Coshan.Disassembler.LLVM
import Coshan.Disassembler.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import qualified Data.Elf as E
import Data.Maybe (fromJust)

readElf :: DisasmTarget -> ByteString -> IO [DisassembledKernel]
readElf target bin = do
  llvm <- getLlvmRef target
  sequence $ disasmSymbol llvm <$> kernelSymbols
  where
    disasmSymbol llvm sym = do
      let section = fromJust $ E.steEnclosingSection sym
      let dataSize = fromIntegral $ if E.steSize sym > 0 then E.steSize sym else E.elfSectionSize section
      let dataOffset = fromIntegral $ E.steValue sym - E.elfSectionAddr section
      let kernelData = BStr.take dataSize $ BStr.drop dataOffset $ E.elfSectionData section
      let (disasmKernelCodeT, disasmInstructionsBin) = BStr.splitAt 256 kernelData
      let disasmKernelName = fromJust $ snd $ E.steName sym
      disasmInstructions <- disassemble llvm disasmInstructionsBin
      pure $ DisassembledKernel {disasmKernelName, disasmKernelCodeT, disasmInstructionsBin, disasmInstructions}
    kernelSymbols = filter isCodeSymbol . head . E.parseSymbolTables $ elf
    isCodeSymbol sym = case E.steEnclosingSection sym of
      Just sct -> E.elfSectionName sct == ".text"
      Nothing -> False
    elf = E.parseElf bin
