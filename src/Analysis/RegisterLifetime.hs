module Analysis.RegisterLifetime where

import           Parser                         ( Instruction(..)
                                                , Operand(..)
                                                )
import           ControlFlow
import           Data.List                      ( nub
                                                , foldl'
                                                )
import           Data.Map.Strict                ( Map(..) )
import qualified Data.Map.Strict               as Map

type UtilizationMap = Map String [RegUtilization]

data RegUtilization = RegWriteSc Int
                    | RegWriteVec Int
                    | RegReadSc Int
                    | RegReadVec Int
  deriving (Eq, Show)

analyzeUtilization :: CFG -> UtilizationMap
analyzeUtilization cfg = Map.fromList $ utilization <$> blocks
 where
  utilization (BasicBlock label insts) = (label, blockUtilization insts)
  (blocks, _) = cfg

blockUtilization :: [Instruction] -> [RegUtilization]
blockUtilization insts = nub $ reads (writes [] insts) insts
 where
  -- register writes
  writes us [] = us
  writes us (AsmInstr _ ((OpSGPR sgprs) : _) : rest) =
    writes ((RegWriteSc <$> sgprs) ++ us) rest
  writes us (AsmInstr _ ((OpVGPR vgprs) : _) : rest) =
    writes ((RegWriteVec <$> vgprs) ++ us) rest
  writes us (_ : rest) = writes us rest
  -- register reads
  reads us []                             = us
  reads us (AsmInstr _ [_       ] : rest) = reads us rest
  reads us (AsmInstr _ (_ : srcs) : rest) = reads (foldl' readsOp us srcs) rest
  reads us (_                     : rest) = reads us rest
  -- instructions can have multiple src operands, we
  -- need to look at each one to determine register reads
  readsOp us (OpSGPR sgprs) = readsSgpr us =<< sgprs
  readsOp us (OpVGPR vgprs) = readsVgpr us =<< vgprs
  readsOp us _              = us
  -- if a register has been written to prior in this basic block,
  -- we do not consider it to be a read dependency
  readsSgpr us r | RegWriteSc r `elem` us = us
                 | otherwise              = RegReadSc r : us
  readsVgpr us r | RegWriteVec r `elem` us = us
                 | otherwise               = RegReadVec r : us
