module Analysis.RegisterLifetime where

import           Parser                         ( Instruction(..)
                                                , Operand(..)
                                                )
import           ControlFlow
import           Data.List                      ( foldl' )

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map(..) )
import qualified Data.Map.Strict               as Map

type UtilizationMap = Map String (Set RegUtilization)

data RegUtilization = SGPRWr Int
                    | VGPRWr Int
                    | SGPRRd Int
                    | VGPRRd Int
  deriving (Eq, Show, Ord)

analyzeUtilization :: CFG -> UtilizationMap
analyzeUtilization cfg = Map.fromList $ utilization <$> blocks
 where
  utilization (BasicBlock label insts) = (label, blockUtilization insts)
  (blocks, _) = cfg

blockUtilization :: [Instruction] -> Set RegUtilization
blockUtilization insts = reads (writes Set.empty insts) insts
 where
  -- register writes
  writes :: Set RegUtilization -> [Instruction] -> Set RegUtilization
  writes us [] = us
  writes us (AsmInstr _ ((OpSGPR sgprs) : _) : rest) =
    writes (Set.union us $ Set.fromList (SGPRWr <$> sgprs)) rest
  writes us (AsmInstr _ ((OpVGPR vgprs) : _) : rest) =
    writes (Set.union us $ Set.fromList (VGPRWr <$> vgprs)) rest
  writes us (_ : rest) = writes us rest
  -- register reads
  reads :: Set RegUtilization -> [Instruction] -> Set RegUtilization
  reads us []                             = us
  reads us (AsmInstr _ [_       ] : rest) = reads us rest
  reads us (AsmInstr _ (_ : srcs) : rest) = reads (foldl' readsOp us srcs) rest
  reads us (_                     : rest) = reads us rest
  -- instructions can have multiple src operands, we
  -- need to look at each one to determine register reads
  -- if a register has been written to before in this basic block,
  -- we do not consider it to be a read dependency
  readsOp us (OpSGPR (r : rest)) | SGPRWr r `Set.member` us = us
                                 | otherwise = Set.insert (SGPRRd r) us
  readsOp us (OpVGPR (r : rest)) | VGPRWr r `Set.member` us = us
                                 | otherwise = Set.insert (VGPRRd r) us
  readsOp us _ = us
