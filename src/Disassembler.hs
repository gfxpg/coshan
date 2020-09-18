{-# LANGUAGE NamedFieldPuns #-}

module Disassembler (readElf, module Disassembler.Types) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BStr
import qualified Data.Elf as E
import Data.List (find)
import Disassembler.LLVM
import Disassembler.Types

import Debug.Trace (trace)

readElf :: DisasmTarget -> ByteString -> IO Disassembly
readElf target bin = do
  llvmRef <- getLlvmRef target
  disasmInstructions <- disassemble llvmRef disasmInstructionsBin
  pure $ Disassembly {disasmKernelCode, disasmInstructionsBin, disasmInstructions}
  where
    (disasmKernelCode, disasmInstructionsBin) = BStr.splitAt 256 elfTextBin
    elfTextBin = E.elfSectionData elfText
    elfText = case find ((== ".text") . E.elfSectionName) (E.elfSections elf) of
      Just section -> section
      Nothing -> error (trace (show $ E.elfSections elf) "No .text section found in ELF file")
    elf = E.parseElf bin
