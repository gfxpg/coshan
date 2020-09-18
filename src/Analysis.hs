{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Analysis (Analyzer, emptyAnalysisCtx, registerAnalyzer, runAnalysis, getAnalyzerOutput) where

import Control.Monad
import Data.List (elem, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeRep)
import Debug.Trace (trace)
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

data AnalysisCtx = AnalysisCtx
  { analyzers :: Map TypeRep (Analyzer Any),
    outputs :: Map TypeRep Any,
    evalStack :: [TypeRep]
  }

emptyAnalysisCtx :: AnalysisCtx
emptyAnalysisCtx = AnalysisCtx {analyzers = Map.empty, outputs = Map.empty, evalStack = []}

registerAnalyzer :: forall a. Typeable a => Analyzer a -> AnalysisCtx -> AnalysisCtx
registerAnalyzer a ctx =
  let newAnalyzers = Map.insert (typeRep $ Proxy @a) (unsafeCoerce a) $ analyzers ctx
   in ctx {analyzers = newAnalyzers}

newtype Analyzer a = Analyzer (AnalysisCtx -> (a, AnalysisCtx))

instance Functor Analyzer where
  fmap = Control.Monad.liftM

instance Applicative Analyzer where
  pure = return
  (<*>) = Control.Monad.ap

instance Monad Analyzer where
  return :: a -> Analyzer a
  return a = Analyzer (\ctx -> (a, ctx))

  (>>=) :: Analyzer a -> (a -> Analyzer b) -> Analyzer b
  (Analyzer as) >>= f = Analyzer $ \s0 ->
    let (a, s1) = as s0
        Analyzer bs = f a
     in bs s1

runAnalysis :: forall a. Typeable a => AnalysisCtx -> a
runAnalysis ctx =
  let Analyzer s = getAnalyzerOutput in fst $ s ctx

getCtx :: Analyzer AnalysisCtx
getCtx = Analyzer $ \s -> (s, s)

modifyCtx :: (AnalysisCtx -> AnalysisCtx) -> Analyzer ()
modifyCtx mod = Analyzer $ \s -> ((), mod s)

getAnalyzerOutput :: forall a. Typeable a => Analyzer a
getAnalyzerOutput = do
  let typeKey = typeRep $ Proxy @a
  AnalysisCtx {outputs, analyzers} <- getCtx
  case Map.lookup typeKey outputs of
    Just o -> pure $ unsafeCoerce o
    Nothing -> case Map.lookup typeKey analyzers of
      Just a -> do
        modifyCtx $ \ctx ->
          let es = evalStack ctx
           in if typeKey `elem` es
                then error $ "Recursive analyzer dependency: " ++ intercalate " -> " (show <$> reverse (typeKey : es))
                else ctx {evalStack = typeKey : es}
        o <- (unsafeCoerce a :: Analyzer a)
        let newOutputs = Map.insert typeKey (unsafeCoerce o) outputs
        modifyCtx $ \ctx -> let (_ : es) = evalStack ctx in ctx {evalStack = es, outputs = newOutputs}
        pure o
      Nothing ->
        error $ "No analyzer could be found with output type " ++ show typeKey
