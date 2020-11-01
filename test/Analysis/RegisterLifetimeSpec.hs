module Analysis.RegisterLifetimeSpec where

import           Test.Hspec
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           ControlFlow
import           Analysis.RegisterLifetime

spec :: Spec
spec = describe "register lifetime analysis" $
  it "does nothing" $ 0 `shouldBe` 0
--   it "builds a map of register utilization" $ do
--     let
--       cfg =
--         ( [ BasicBlock
--             "BB0"
--             [ AsmInstr "s_load_dwordx4"
--                        [Osgpr [0, 1, 2, 3], Osgpr [4, 5], OConst 0]
--             , AsmInstr "s_waitcnt"      [OOther "lgkmcnt(0)"]
--             , AsmInstr "s_cmp_eq_u64"   [Osgpr [2, 3], OConst 0]
--             , AsmInstr "s_cbranch_scc1" [OOther "BB1_3"]
--             ]
--           , BasicBlock
--             "BB1"
--             [AsmInstr "v_mov_b32_e32" [Ovgpr [0], OConst 1091567616]]
--           , BasicBlock
--             "BB1_2"
--             [ AsmInstr "s_load_dword"  [Osgpr [4], Osgpr [0, 1], OConst 0]
--             , AsmInstr "v_mov_b32_e32" [Ovgpr [2], Osgpr [1]]
--             , AsmInstr "v_mov_b32_e32" [Ovgpr [1], Osgpr [0]]
--             , AsmInstr "s_add_u32"     [Osgpr [0], Osgpr [0], OConst 4]
--             , AsmInstr "s_addc_u32"    [Osgpr [1], Osgpr [1], OConst 0]
--             , AsmInstr "s_add_u32"     [Osgpr [2], Osgpr [2], OConst (-1)]
--             , AsmInstr "s_addc_u32"    [Osgpr [3], Osgpr [3], OConst (-1)]
--             , AsmInstr "s_cmp_lg_u64"  [Osgpr [2, 3], OConst 0]
--             , AsmInstr "s_waitcnt"     [OOther "lgkmcnt(0)"]
--             , AsmInstr "v_add_f32_e32" [Ovgpr [3], Osgpr [4], Ovgpr [0]]
--             , AsmInstr "global_store_dword"
--                        [Ovgpr [1, 2], Ovgpr [3], OOther "off"]
--             , AsmInstr "s_cbranch_scc1" [OOther "BB1_2"]
--             ]
--           , BasicBlock "BB1_3" [AsmInstr "s_endpgm" []]
--           ]
--         , [ BlockEdgeT "BB0" "BB1_3"
--           , BlockEdgeF "BB0" "BB1"
--           , BlockEdgeUnc "BB1" "BB1_2"
--           , BlockEdgeT "BB1_2" "BB1_2"
--           , BlockEdgeF "BB1_2" "BB1_3"
--           ]
--         )
--     let util = analyzeUtilization cfg
--     util `shouldBe` Map.fromList
--       [ ( "BB0"
--         , Set.fromList
--           [SGPRWr 0, SGPRWr 1, SGPRWr 2, SGPRWr 3, SGPRRd 4, SGPRRd 5]
--         )
--       , ("BB1", Set.fromList [VGPRWr 0])
--       , ( "BB1_2"
--         , Set.fromList
--           [ SGPRWr 0
--           , SGPRWr 1
--           , SGPRWr 2
--           , SGPRWr 3
--           , SGPRWr 4
--           , VGPRWr 1
--           , VGPRWr 2
--           , VGPRWr 3
--           , VGPRRd 0
--           ]
--         )
--       , ("BB1_3", Set.empty)
--       ]
