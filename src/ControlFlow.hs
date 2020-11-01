module ControlFlow
  ( constructGraph,
    BasicBlock (..),
    BlockEdge (..),
    CFG,
  )
where

import Data.List
import Parser
  ( Instruction (..),
    Operand (..),
  )

-- label, instructions
data BasicBlock = BasicBlock String [Instruction]
  deriving (Eq, Show, Read)

data BlockEdge
  = BlockEdgeT String String
  | BlockEdgeF String String
  | BlockEdgeUnc String String
  deriving (Eq, Show, Read)

type CFG = ([BasicBlock], [BlockEdge])

constructGraph :: [Instruction] -> CFG
constructGraph _ = ([], [])

-- scanBlocks :: CFG -> Int -> [Instruction] -> CFG
-- scanBlocks (blocks, edges) i insts = case scanBasicBlock i insts of
--   (block, []  ) -> (block : blocks, edges)
--   (block, rest) -> (block : newBlocks, allEdges)
--    where
--     (newBlocks, newEdges) = scanBlocks (blocks, edges) (i + 1) rest
--     allEdges              = case last bbInsts of
--       (AsmInstr _ [OOther branchName]) ->
--         BlockEdgeT bbName branchName : BlockEdgeF bbName nextBbName : newEdges
--       _ -> BlockEdgeUnc bbName nextBbName : newEdges
--     BasicBlock nextBbName _ :      _       = newBlocks
--     BasicBlock              bbName bbInsts = block

-- scanBasicBlock :: Int -> [Instruction] -> (BasicBlock, [Instruction])
-- scanBasicBlock id insts = (BasicBlock name block, rest)
--  where
--   (block, rest) = case break isBranch body of
--     (block, br@(AsmInstr _ _) : rest) -> (block ++ [br], rest)
--     (block, rest                    ) -> (block, rest)
--   (name, body) = case insts of
--     (AsmLabel label) : rest -> (label, rest)
--     _                       -> (blockName id, insts)

-- blockName :: Int -> String
-- blockName i = "BB" ++ show i

-- isBranch :: Instruction -> Bool
-- -- EXEC-modifying instructions are not recognized as branches for now
-- isBranch (AsmInstr ('v' : _) _) = False
-- isBranch (AsmInstr inst      _) = "s_cbranch" `isPrefixOf` inst
-- isBranch (AsmLabel _          ) = True
