module Parser.Types where

newtype Listing = Listing [Instruction]
  deriving (Eq, Show, Read)

data Instruction = Instruction String [Operand]
  deriving (Eq, Show, Read)

data Operand = OpSGPR [Int]
             | OpVGPR [Int]
             | OpConst Int
             | OpSys String
  deriving (Eq, Show, Read)
