module Happy.Tabular.Main (
    TabularArgs(..), TabularResult, runTabular
  ) where

import Happy.Grammar
import Happy.Tabular.Tables
import qualified Happy.Tabular.LALR as LALR
import Happy.Tabular.FindRedundancies
import Happy.Tabular.Info
import Happy.Tabular
import Data.Array (Array)
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Control.Monad
import Data.Version (Version)

-------- Main entry point (runTabular) --------

data TabularArgs = TabularArgs {
  inFile :: String, -- printed to the info file, not used otherwise
  infoFile :: Maybe String,

  dumpLR0 :: Bool,
  dumpLA :: Bool,
  dumpAction :: Bool,
  dumpGoto :: Bool
}

type TabularResult = (ActionTable, GotoTable, [LALR.Lr1State], [Int])

runTabular :: Bool -> TabularArgs -> Grammar -> Version -> IO TabularResult
runTabular glr args g version = do
   let select_reductions =
         if glr
         then select_all_reductions
         else select_first_reduction

   let tables      = genTables select_reductions g
       sets        = lr0items tables
       la          = lookaheads tables
       items2      = lr1items tables
       goto        = gotoTable tables
       action      = actionTable tables
       (conflictArray, (sr,rr)) = conflicts tables
   optPrint (dumpLR0 args) (print sets)
   optPrint (dumpLA args) (print la)
   optPrint (dumpAction args) (print action)
   optPrint (dumpGoto args) (print goto)
   reportUnusedRules tables
   let (unused_rules, unused_terminals) = redundancies tables
   writeInfoFile sets g action goto conflictArray (inFile args) (infoFile args) unused_rules unused_terminals version
   reportConflicts g sr rr
   return (action, goto, items2, unused_rules)
 where
   optPrint b io = when b (putStr "\n---------------------\n" >> io)

-------- Helpers --------

reportUnusedRules :: Tables -> IO ()
reportUnusedRules tables = do
    let (unused_rules, unused_terminals) = redundancies tables
    when (not (null unused_rules)) $
        hPutStrLn stderr ("unused rules: " ++ show (length unused_rules))
    when (not (null unused_terminals)) $
        hPutStrLn stderr ("unused terminals: " ++ show (length unused_terminals))

reportConflicts :: Grammar -> Int -> Int -> IO ()
reportConflicts g sr rr = case expect g of
    Just n | n == sr && rr == 0 -> return ()
    Just _ | rr > 0 ->
        die $ "The grammar has reduce/reduce conflicts.\n" ++
              "This is not allowed when an expect directive is given\n"
    Just _ ->
        die $ "The grammar has " ++ show sr ++ " shift/reduce conflicts.\n" ++
              "This is different from the number given in the expect directive\n"
    _ -> do
        if sr /= 0
          then hPutStrLn stderr ("shift/reduce conflicts:  " ++ show sr)
          else return ()

        if rr /= 0
          then hPutStrLn stderr ("reduce/reduce conflicts: " ++ show rr)
          else return ()

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

writeInfoFile
    :: [LALR.ItemSetWithGotos]
    -> Grammar
    -> ActionTable
    -> GotoTable
    -> Array Int (Int,Int)
    -> String
    -> Maybe String
    -> [Int]
    -> [String]
    -> Version
    -> IO ()
writeInfoFile sets g action goto conflictArray file info_file unused_rules unused_terminals version =
    let info = genInfoFile
          (map fst sets)
          g
          action
          goto
          (token_specs g)
          conflictArray
          file
          unused_rules
          unused_terminals
          version
    in case info_file of
            Just s -> writeFile s info >> hPutStrLn stderr ("Grammar info written to: " ++ s)
            Nothing -> return ()
