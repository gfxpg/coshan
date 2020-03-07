module ControlFlow
  ( constructGraph
  )
where

import           Data.List
import           Parser.Types                   ( Instruction(..) )

-- label, instructions
data BasicBlock = BasicBlock String [Instruction]
  deriving (Show)

data BlockEdge = BlockEdgeT String
               | BlockEdgeF String
  deriving (Show)

type CFG = ([BasicBlock], [BlockEdge])

constructGraph :: [Instruction] -> CFG
constructGraph is = ([firstBlock], [])
  where (firstBlock, _) = scanBasicBlock is 0

scanBasicBlock :: [Instruction] -> Int -> (BasicBlock, [Instruction])
scanBasicBlock insts id = (BasicBlock name block, rest)
 where
  (block, rest) = break branchInst body
  (name , body) = case insts of
    (AsmLabel label) : rest -> (label, rest)
    _                       -> (show id, insts)

branchInst :: Instruction -> Bool
-- EXEC-modifying instructions are not recognized as branches for now
branchInst (AsmInstr ('v' : _) _) = False
branchInst (AsmInstr inst      _) = "s_cbranch" `isPrefixOf` inst
branchInst _                      = False
