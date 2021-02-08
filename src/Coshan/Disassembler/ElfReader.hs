{-# LANGUAGE OverloadedStrings #-}

module Coshan.Disassembler.ElfReader (readElf) where

import Control.Monad (forM)
import Coshan.Disassembler.LLVM
import Coshan.Disassembler.Types
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Elf as E
import Data.List (find)

readElf :: DisasmTarget -> ByteString -> IO [DisassembledKernel]
readElf target bin = do
  llvm <- getLlvmRef target
  forM kernels $ \k -> do
    dasm <- disassemble llvm (disasmInstructionsBin k)
    pure $ k {disasmInstructions = dasm}
  where
    kernels = findKernels textSection globalSymbols
    Just textSection = find ((== ".text") . E.elfSectionName) $ E.elfSections elf
    globalSymbols = head $ E.parseSymbolTables elf
    elf = E.parseElf bin

findKernels :: E.ElfSection -> [E.ElfSymbolTableEntry] -> [DisassembledKernel]
findKernels textSection = extractKernels [] . foldr findKernels ([], [])
  where
    -- ([(kernelName, kernelOffset, Maybe kernelSize)], [kernelDescriptorName, kernelDescriptorData])
    extractKernels :: [DisassembledKernel] -> ([(ByteString, Int, Maybe Int)], [(ByteString, ByteString)]) -> [DisassembledKernel]
    extractKernels acc ([], _) = acc
    extractKernels acc ((kName, kOffset, kMaybeSize) : rest, kdescs) =
      let kSize = case rest of
            _ | Just size <- kMaybeSize -> size
            ((_, nextOffset, _) : _) -> nextOffset - kOffset
            [] -> fromIntegral $ E.elfSectionSize textSection
          kBin = BC8.take kSize $ BC8.drop kOffset $ E.elfSectionData textSection
          (kDesc, kInstrs) = case find ((== kName) . fst) kdescs of
            Just (_, kd) -> (KernelDescriptorV3 kd, kBin)
            _ -> KernelDescriptorV2 `first` BC8.splitAt 256 kBin
          acc' = acc ++ [DisassembledKernel {disasmKernelName = kName, disasmKernelCodeT = kDesc, disasmInstructionsBin = kInstrs, disasmInstructions = []}]
       in extractKernels acc' (rest, kdescs)
    findKernels sym (ktexts, kdescs)
      | Just symSection <- E.steEnclosingSection sym,
        (_, Just symName) <- E.steName sym =
        case E.elfSectionName symSection of
          ".text" ->
            let offset = fromIntegral $ E.steValue sym - E.elfSectionAddr symSection
                symSize = fromIntegral $ E.steSize sym
                size = if symSize > 0 then Just symSize else Nothing
             in ((symName, offset, size) : ktexts, kdescs)
          ".rodata"
            | Just kdKernel <- BC8.stripSuffix ".kd" symName ->
              let kdOffset = fromIntegral $ E.steValue sym - E.elfSectionAddr symSection
                  kdSize = fromIntegral $ E.steSize sym -- should always be 64
                  kdData = BC8.take kdSize $ BC8.drop kdOffset $ E.elfSectionData symSection
               in if kdSize /= 64
                    then error $ "Kernel descriptor symbol " ++ BC8.unpack symName ++ " has unexpected size of " ++ show kdSize ++ " (expected 64)"
                    else (ktexts, (kdKernel, kdData) : kdescs)
          _ -> (ktexts, kdescs)
      | otherwise = (ktexts, kdescs)
