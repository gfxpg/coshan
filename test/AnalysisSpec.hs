{-# LANGUAGE ScopedTypeVariables #-}

module AnalysisSpec where

import Control.Exception (evaluate)
import Coshan.Analysis
import Debug.Trace (trace)
import Test.Hspec

newtype Res = Res Int deriving (Show, Eq)

newtype Res2 = Res2 Int deriving (Show, Eq)

newtype ResInfRec = ResInfRec Int deriving (Show, Eq)

newtype ResInfRec2 = ResInfRec2 Int deriving (Show, Eq)

spec :: Spec
spec = describe "analysis" $ do
  it "resolves dependencies" $ do
    let ctx = registerAnalyzer analyzeRes2 $ registerAnalyzer analyzeRes $ emptyAnalysisCtx
    let res :: Res = runAnalysis ctx
    res `shouldBe` (Res 3)
  it "detects recursive dependencies" $ do
    let ctx = registerAnalyzer analyzeResInfRec2 $ registerAnalyzer analyzeResInfRec $ emptyAnalysisCtx
    evaluate (runAnalysis ctx :: ResInfRec) `shouldThrow` errorCall "Recursive analyzer dependency: ResInfRec -> ResInfRec2 -> ResInfRec"
    evaluate (runAnalysis ctx :: ResInfRec2) `shouldThrow` errorCall "Recursive analyzer dependency: ResInfRec2 -> ResInfRec -> ResInfRec2"
  it "reports unknown analyzers" $ do
    evaluate (runAnalysis emptyAnalysisCtx :: Res) `shouldThrow` errorCall "No analyzer could be found with output type Res"

analyzeRes :: Analyzer Res
analyzeRes = do
  res2 :: Res2 <- trace "evaluating Res" getAnalyzerOutput
  res2again :: Res2 <- trace "requesting Res2 again" getAnalyzerOutput
  pure $ trace ("got res2 (" ++ show res2 ++ ", " ++ show res2again ++ ")") (Res 3)

analyzeRes2 :: Analyzer Res2
analyzeRes2 = pure $ trace "evaluating Res2" (Res2 313)

analyzeResInfRec :: Analyzer ResInfRec
analyzeResInfRec = do
  ResInfRec2 r <- trace "evaluating ResInfRec" getAnalyzerOutput
  pure (trace "returning from ResInfRec" (ResInfRec r))

analyzeResInfRec2 :: Analyzer ResInfRec2
analyzeResInfRec2 = do
  ResInfRec r <- trace "evaluating ResInfRec2 (infinite recursion trigger)" getAnalyzerOutput
  pure (trace "returning from ResInfRec2" (ResInfRec2 r))
