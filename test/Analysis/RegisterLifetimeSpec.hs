module Analysis.RegisterLifetimeSpec where

import           Test.Hspec
import qualified Data.Map.Strict               as Map
import           Helper                         ( runControlFlowCase )
import           Parser                         ( Instruction(..)
                                                , Operand(..)
                                                )
import           ControlFlow
import           Analysis.RegisterLifetime

spec :: Spec
spec = describe "register lifetime analysis" $ do
  it "builds a map of register utilization" $ do
    let
      cfg =
        ( [ BasicBlock
            "BB0"
            [ AsmInstr "s_load_dwordx4"
                       [OpSGPR [0, 1, 2, 3], OpSGPR [4, 5], OpConst 0]
            , AsmInstr "s_waitcnt"      [OpSys "lgkmcnt(0)"]
            , AsmInstr "s_cmp_eq_u64"   [OpSGPR [2, 3], OpConst 0]
            , AsmInstr "s_cbranch_scc1" [OpSys "BB1_3"]
            ]
          , BasicBlock
            "BB1"
            [AsmInstr "v_mov_b32_e32" [OpVGPR [0], OpConst 1091567616]]
          , BasicBlock
            "BB1_2"
            [ AsmInstr "s_load_dword"  [OpSGPR [4], OpSGPR [0, 1], OpConst 0]
            , AsmInstr "v_mov_b32_e32" [OpVGPR [2], OpSGPR [1]]
            , AsmInstr "v_mov_b32_e32" [OpVGPR [1], OpSGPR [0]]
            , AsmInstr "s_add_u32"     [OpSGPR [0], OpSGPR [0], OpConst 4]
            , AsmInstr "s_addc_u32"    [OpSGPR [1], OpSGPR [1], OpConst 0]
            , AsmInstr "s_add_u32"     [OpSGPR [2], OpSGPR [2], OpConst (-1)]
            , AsmInstr "s_addc_u32"    [OpSGPR [3], OpSGPR [3], OpConst (-1)]
            , AsmInstr "s_cmp_lg_u64"  [OpSGPR [2, 3], OpConst 0]
            , AsmInstr "s_waitcnt"     [OpSys "lgkmcnt(0)"]
            , AsmInstr "v_add_f32_e32" [OpVGPR [3], OpSGPR [4], OpVGPR [0]]
            , AsmInstr "global_store_dword"
                       [OpVGPR [1, 2], OpVGPR [3], OpSys "off"]
            , AsmInstr "s_cbranch_scc1" [OpSys "BB1_2"]
            ]
          , BasicBlock "BB1_3" [AsmInstr "s_endpgm" []]
          ]
        , [ BlockEdgeT "BB0" "BB1_3"
          , BlockEdgeF "BB0" "BB1"
          , BlockEdgeUnc "BB1" "BB1_2"
          , BlockEdgeT "BB1_2" "BB1_2"
          , BlockEdgeF "BB1_2" "BB1_3"
          ]
        )
    let util = analyzeUtilization cfg
    util `shouldBe` Map.empty
