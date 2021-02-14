{-# LANGUAGE LambdaCase #-}

module Coshan.ControlFlow (buildCfg, CFG (..), BasicBlock (..), BasicBlockIdx) where

import Control.Monad ((>=>))
import Coshan.ControlFlow.Types
import Coshan.Disassembler (Instruction (..), Operand (..), PC)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

-- TODO: Build CFG during disassembly: when parsing an instruction, check if it's a branch, and if so, follow it
-- instead of assuming that each succeeding instruction is valid.

buildCfg :: [(PC, Instruction)] -> CFG
buildCfg instructions =
  let branches = scanBranchInstructions instructions
      leaders = scanBlockLeaders branches
   in CFG $ fillPredecessors leaders branches $ splitInstructionsIntoBlocks leaders branches instructions

fillPredecessors :: Set PC -> Map PC (BasicBlockExitPoint PC) -> [BasicBlock] -> [BasicBlock]
fillPredecessors blockLeaders branches blocks = blocks'
  where
    blocks' = (\(bbIdx, bb) -> bb {bbEntries = Map.findWithDefault [] bbIdx predecessors}) <$> blocksWithIndex
    blocksWithIndex = zip [0 ..] blocks
    predecessors :: Map BasicBlockIdx [BasicBlockIdx]
    predecessors = foldr go Map.empty blocksWithIndex
      where
        go (bbIdx, bb) acc = case bbExit bb of
          BbExitFallThrough succBbIdx -> Map.insertWith (++) succBbIdx [bbIdx] acc
          BbExitJump succBbIdx -> Map.insertWith (++) succBbIdx [bbIdx] acc
          BbExitJumpSavePc sgprPair succBbIdx ->
            let returnToBbIdx = bbIdx + 1
                (startPc, _) = head $ bbInstructions $ blocks !! succBbIdx
                returnFromPcs = followCallToReturnPcs sgprPair startPc
                returnFromBbIdxs = catMaybes $ (\pc -> (Set.lookupLE pc >=> (`Set.lookupIndex` blockLeaders)) blockLeaders) <$> returnFromPcs
             in Map.insertWith (++) returnToBbIdx returnFromBbIdxs $
                  Map.insertWith (++) succBbIdx [bbIdx] acc
          BbExitCondJump succBbIdx1 succBbbIdx2 -> Map.insertWith (++) succBbIdx1 [bbIdx] $ Map.insertWith (++) succBbbIdx2 [bbIdx] acc
          _ -> acc
    followCallToReturnPcs :: SgprPair -> PC -> [PC]
    followCallToReturnPcs callGprs currentPc =
      let closestRet = find (\(retPc, retGprs) -> retPc >= currentPc && retGprs == callGprs) returnPoints
          closestBr = Map.lookupGE currentPc branchPoints
       in case (closestRet, closestBr) of
            (Nothing, _) ->
              [] -- TODO: this should probably be a warning (no s_setpc following s_call)
            (Just (retPc, _), Just (brPc, brTargets))
              | retPc > brPc ->
                nub $ followCallToReturnPcs callGprs =<< brTargets
            (Just (retPc, _), _) ->
              [retPc]
    returnPoints = Map.toAscList $ Map.mapMaybe (\case BbExitDynamic sgprPair -> Just sgprPair; _ -> Nothing) branches
    branchPoints = Map.mapMaybe (\case BbExitJump pc -> Just [pc]; BbExitCondJump pc1 pc2 -> Just [pc1, pc2]; _ -> Nothing) branches

splitInstructionsIntoBlocks :: Set PC -> Map PC (BasicBlockExitPoint PC) -> [(PC, Instruction)] -> [BasicBlock]
splitInstructionsIntoBlocks blockLeaders branches instructions = blocks
  where
    blocks = {- trace ("starts: " ++ show blockStarts') $-} reverse $ makeBlocks [] instructions $ Set.toAscList blockLeaders
    makeBlocks blocks [] _ = blocks
    makeBlocks blocks remInstructions (_ : nextStartPc : rest) =
      makeBlocks (BasicBlock currInsts [] exitPoint : blocks) nextBbInsts (nextStartPc : rest)
      where
        (currInsts, nextBbInsts) = break ((== nextStartPc) . fst) remInstructions
        (bbEndPc, _) = last currInsts
        exitPoint = case Map.lookup bbEndPc branches of
          Just (BbExitJump targetPc)
            | Just targetBb <- Set.lookupIndex targetPc blockLeaders -> BbExitJump targetBb
          Just (BbExitCondJump targetPc1 targetPc2)
            | Just targetBb1 <- Set.lookupIndex targetPc1 blockLeaders,
              Just targetBb2 <- Set.lookupIndex targetPc2 blockLeaders ->
              BbExitCondJump targetBb1 targetBb2
          Just (BbExitJumpSavePc sgprPair targetPc)
            | Just targetBb <- Set.lookupIndex targetPc blockLeaders -> BbExitJumpSavePc sgprPair targetBb
          Just (BbExitDynamic sgprPair) -> BbExitDynamic sgprPair
          Nothing -> case Set.lookupGT bbEndPc blockLeaders of
            Just succPc | Just succIdx <- Set.lookupIndex succPc blockLeaders -> BbExitFallThrough succIdx
            _ -> BbExitTerminal
          Just _ -> undefined
    makeBlocks blocks remInstructions _ = BasicBlock remInstructions [] BbExitTerminal : blocks

scanBlockLeaders :: Map PC (BasicBlockExitPoint PC) -> Set PC
scanBlockLeaders =
  Map.foldlWithKey'
    ( \s fromPc exitPoint -> Set.insert (fromPc + 4) $ case exitPoint of
        BbExitJump toPc -> Set.insert toPc s
        BbExitCondJump toPc1 toPc2 -> Set.insert toPc1 $ Set.insert toPc2 s
        BbExitJumpSavePc _ toPc -> Set.insert toPc s
        _ -> s
    )
    (Set.singleton 0)

scanBranchInstructions :: [(PC, Instruction)] -> Map PC (BasicBlockExitPoint PC)
scanBranchInstructions instructions = go instructions Map.empty
  where
    go [] acc = acc
    go ((pc, i) : rest) acc = go rest $ case i of
      Instruction ["s", "branch"] [OConst offset] ->
        Map.insert pc (BbExitJump (shortJumpTarget pc offset)) acc
      Instruction ("s" : "cbranch" : _) [OConst offset] ->
        Map.insert pc (BbExitCondJump (shortJumpTarget pc offset) (pc + 4)) acc
      Instruction ("s" : "call" : _) [Osgpr [s1, s2], OConst offset] ->
        Map.insert pc (BbExitJumpSavePc (SgprPair (s1, s2)) (shortJumpTarget pc offset)) acc
      Instruction ("s" : "setpc" : _) [Osgpr [s1, s2]] ->
        Map.insert pc (BbExitDynamic (SgprPair (s1, s2))) acc
      _ ->
        acc

shortJumpTarget :: PC -> Int -> PC
shortJumpTarget pc offset
  | offset <= 32767 = pc + 4 + 4 * offset
  | otherwise = pc + 4 + 4 * ((-1) * (65536 - offset))
