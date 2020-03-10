module Analysis.RegisterLifetime where

import           ControlFlow
import           Data.Map.Strict                ( Map(..) )
import qualified Data.Map.Strict               as Map

type UtilizationMap = Map String [RegUtilization]

data RegUtilization = RegWrite Int
                    | RegRead Int
  deriving (Eq, Show)

analyzeUtilization :: CFG -> UtilizationMap
analyzeUtilization _ = Map.empty
