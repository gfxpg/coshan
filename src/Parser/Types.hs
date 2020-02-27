module Parser.Types where

import           Data.Map.Strict                ( Map(..) )

data Listing = Listing KernelCodeT [Instruction]
  deriving (Eq, Show, Read)

newtype KernelCodeT = KernelCodeT (Map String Int)
  deriving (Eq, Show, Read)

data Instruction = AsmInstr String [Operand]
                 | AsmLabel String
  deriving (Eq, Show, Read)

data Operand = OpSGPR [Int]
             | OpVGPR [Int]
             | OpVCC
             | OpSCC
             | OpExec
             | OpConst Int
             | OpSys String
  deriving (Eq, Show, Read)
