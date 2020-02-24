module Parser.Types where

data Listing = Listing [Directive] [Instruction]
  deriving (Eq, Show, Read)

data Directive = Directive String String
  deriving (Eq, Show, Read)

data Instruction = Instruction String [Operand]
  deriving (Eq, Show, Read)

data Operand = OpSGPR [Int]
             | OpVGPR [Int]
             | OpConst Int
             | OpSys String
  deriving (Eq, Show, Read)
