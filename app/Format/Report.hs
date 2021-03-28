module Format.Report where

import Coshan.Disassembler
import qualified Coshan.Reporting as R
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.List (intersperse)
import Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.Printf (printf)

printAnalysisReport :: DisassembledKernel -> [(String, [R.LogMessage])] -> IO ()
printAnalysisReport kernel = P.putDoc . (<> P.hardline) . catWithBreak . (printReport kernel <$>)

putBreak :: P.Doc
putBreak = P.hardline <> P.hardline

catWithBreak :: [P.Doc] -> P.Doc
catWithBreak = mconcat . intersperse putBreak

printReport :: DisassembledKernel -> (String, [R.LogMessage]) -> P.Doc
printReport _ (analyzer, []) = P.bold (P.green "[ OK ]" <+> P.text analyzer)
printReport kernel (analyzer, errors) = catWithBreak (putMessage <$> zip [1 ..] errors)
  where
    putMessage (i, e) =
      let header = P.bold (P.red ("[ " <> P.int i <> "/" <> P.int (length errors) <> " ]") <+> P.text analyzer)
       in P.vcat [header, putError kernel e]

putError :: DisassembledKernel -> R.LogMessage -> P.Doc
putError kernel (R.LogMessage pc error) = case error of
  R.InstructionRequired {R.instreqInstruction = i, R.instreqBacktrace = bt, R.instreqExplanation = expl} ->
    P.vcat
      [ P.bold "Problem:" <+> "Missing instruction" <+> (P.bold . P.magenta . putInstruction) i,
        P.bold "Explanation:" <+> P.text expl,
        P.bold "Location:",
        putTrace kernel (pc, Nothing),
        P.bold "Backtrace:",
        (catWithBreak . (putTrace kernel <$>)) bt
      ]
  R.CounterWaitRequired {R.ctrreqWaitClause = clause, R.ctrreqSucceedingEvents = succ, R.ctrreqPrecedingEvents = pred, R.ctrreqExplanation = expl} ->
    P.vcat $
      [ P.bold "Problem:" <+> "Missing" <+> (P.bold . P.magenta . putInstruction) (Instruction ["s", "waitcnt"] [clause]),
        P.bold "Explanation:" <+> P.text expl,
        P.bold "Location:",
        putTrace kernel (pc, Nothing)
      ]
        ++ ( case succ of
               [] -> []
               ops -> [P.bold "Newer unwaited operations in the queue:", (catWithBreak . (putTrace kernel <$>)) ops]
           )
        ++ ( case pred of
               [op] ->
                 [ P.bold "Memory operation:",
                   putTrace kernel op
                 ]
               op : pred ->
                 [ P.bold "Memory operation:",
                   putTrace kernel op,
                   P.bold "Earlier unwaited operations in the queue:",
                   (catWithBreak . (putTrace kernel <$>)) pred
                 ]
               _ -> []
           )

putTrace :: DisassembledKernel -> (PC, a) -> P.Doc
putTrace kernel (pc, _) = P.vcat (putInstW <$> window)
  where
    window = instructionWindow (disasmInstructions kernel) pc
    putInstW (instPc, i) =
      let fmt = if instPc == pc then P.bold . P.cyan else id
       in fmt $ P.text (printf "%16x" instPc) <+> P.text (BC8.unpack i)

instructionWindow :: [(PC, ByteString)] -> PC -> [(PC, ByteString)]
instructionWindow instructions pc = case instructions of
  (i1@(pc1, _) : i2@(pc2, _) : i3 : _) | pc1 == pc || pc2 == pc -> [i1, i2, i3]
  [i1@(pc1, _), i2@(pc2, _)] | pc1 == pc || pc2 == pc -> [i1, i2]
  [i1@(pc1, _)] | pc1 == pc -> [i1]
  (_ : rest) -> instructionWindow rest pc
  _ -> error $ "Instruction " <> show pc <> " not found"

putInstruction :: Instruction -> P.Doc
putInstruction (Instruction opparts ops) = opcode <+> foldr ((<+>) . putOperand) "" ops
  where
    opcode = P.text $ BC8.unpack $ BC8.intercalate "_" opparts
    putOperand o = case o of
      Osgpr rs -> P.char 's' <> putRegs rs
      Ovgpr rs -> P.char 'v' <> putRegs rs
      Ottmp rs -> "ttmp" <> putRegs rs
      Ovmcnt ctr -> "vmcnt(" <> P.int ctr <> P.char ')'
      Olgkmcnt ctr -> "lgkmcnt(" <> P.int ctr <> P.char ')'
      Oexpcnt ctr -> "expcnt(" <> P.int ctr <> P.char ')'
      OConst int -> P.int int
      OConstF f -> P.float f
      OCtrl ctrl -> P.text $ BC8.unpack ctrl
    putRegs [i] = P.int i
    putRegs is
      | from <- head is,
        to <- last is,
        from == minimum is,
        to == maximum is =
        P.char '[' <> P.int from <> P.char ':' <> P.int to <> P.char ']'
      | otherwise = error $ "Cannot print register range " ++ show is
