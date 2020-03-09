module ControlFlow
  ( constructGraph
  )
where

import           Data.List
import           Parser.Types                   ( Instruction(..)
                                                , Operand(..)
                                                )

-- label, instructions
data BasicBlock = BasicBlock String [Instruction]
  deriving (Eq, Show, Read)

data BlockEdge = BlockEdgeT String String
               | BlockEdgeF String String
  deriving (Eq, Show, Read)

type CFG = ([BasicBlock], [BlockEdge])

constructGraph :: [Instruction] -> CFG
constructGraph = scanBlocks ([], []) 0

scanBlocks :: CFG -> Int -> [Instruction] -> CFG
scanBlocks (blocks, edges) i insts = case scanBasicBlock i insts of
  (block, []  ) -> (block : blocks, edges)
  (block, rest) -> (block : newBlocks, e1 : e0 : edges)
   where
    (newBlocks, newEdges)     = scanBlocks (blocks, edges) (i + 1) rest
    e1                        = BlockEdgeT bbName $ branchLabel $ last bbInsts
    e0                        = BlockEdgeF bbName $ blockName $ i + 1
    BasicBlock bbName bbInsts = block
    branchLabel (AsmInstr _ [OpSys label]) = label

scanBasicBlock :: Int -> [Instruction] -> (BasicBlock, [Instruction])
scanBasicBlock id insts = (BasicBlock name block, rest)
 where
  (block, rest) = case break isBranch body of
    (block, br@(AsmInstr _ _) : rest) -> (block ++ [br], rest)
    (block, rest                    ) -> (block, rest)
  (name, body) = case insts of
    (AsmLabel label) : rest -> (label, rest)
    _                       -> (blockName id, insts)

blockName :: Int -> String
blockName i = "BB" ++ show i

isBranch :: Instruction -> Bool
-- EXEC-modifying instructions are not recognized as branches for now
isBranch (AsmInstr ('v' : _) _) = False
isBranch (AsmInstr inst      _) = "s_cbranch" `isPrefixOf` inst
isBranch (AsmLabel _          ) = True
