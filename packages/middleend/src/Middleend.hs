module Middleend(
    mkFirst, genLR0Items, genLookaheads, genLR1States, genActionTable, genGotoTable, countConflicts,
    Lr0State, Lr1State, LookaheadInfo, ActionTable, GotoTable,
    MiddleendArgs(..), MiddleendResult, runMiddleend
  ) where

import First
import qualified LALR
import LALR (Lr0Item, Lr1Item, precalcClosure0, propLookaheads, calcLookaheads, mergeLookaheadInfo)
import NameSet (NameSet)
import Grammar
import GenUtils
import FindRedundancies
import Info
import Data.Set (Set)
import Data.Array (Array)
import System.IO
import Control.Monad

-------- Pure middleend functions, may be called without creating MiddleendArgs --------

type Lr0State = (Set Lr0Item, [(Name, Int)])
type Lr1State = ([Lr1Item], [(Name, Int)])
type LookaheadInfo = Array Int [(Lr0Item, NameSet)]

genLR0Items :: Grammar -> [Lr0State]
genLR0Items g = LALR.genLR0items g (precalcClosure0 g)

genLookaheads :: Grammar -> [Lr0State] -> ([Name] -> NameSet) -> LookaheadInfo
genLookaheads g sets first =
    let (spont, prop) = propLookaheads g sets first in
        calcLookaheads (length sets) spont prop

genLR1States :: LookaheadInfo -> [Lr0State] -> [Lr1State]
genLR1States = mergeLookaheadInfo

genActionTable :: Grammar -> ([Name] -> NameSet) -> [Lr1State] -> ActionTable
genActionTable = LALR.genActionTable

genGotoTable :: Grammar -> [Lr0State] -> GotoTable
genGotoTable = LALR.genGotoTable

countConflicts :: ActionTable -> (Array Int (Int, Int), (Int, Int))
countConflicts = LALR.countConflicts

-------- Main entry point (runMiddleend) --------

data MiddleendArgs = MiddleendArgs {
  inFile :: String, -- only used as information inside the info file
  infoFile :: Maybe String,

  dumpLR0 :: Bool,
  dumpLA :: Bool,
  dumpAction :: Bool,
  dumpGoto :: Bool
}

type MiddleendResult = (ActionTable, GotoTable, [Lr1State], [Int])

runMiddleend :: MiddleendArgs -> Grammar -> IO MiddleendResult
runMiddleend args g = 
   let first       = mkFirst g
       sets        = genLR0Items g
       la          = genLookaheads g sets first
       items2      = genLR1States la sets
       goto        = genGotoTable g sets
       action      = genActionTable g first items2
       (conflictArray, (sr,rr)) = (countConflicts action)
    in do
        optPrint (dumpLR0 args) (print sets)
        optPrint (dumpLA args) (print la)
        optPrint (dumpAction args) (print action)
        optPrint (dumpGoto args) (print goto)
        (unused_rules, unused_terminals) <- reportUnusedRules g action
        writeInfoFile sets g action goto conflictArray (inFile args) (infoFile args) unused_rules unused_terminals
        reportConflicts g sr rr
        return (action, goto, items2, unused_rules)

-------- Helpers --------

reportUnusedRules :: Grammar -> ActionTable -> IO ([Int], [String])
reportUnusedRules g action = 
    let result@(unused_rules, unused_terminals) = find_redundancies first_reduction g action in do
        when (not (null unused_rules)) $ hPutStrLn stderr ("unused rules: " ++ show (length unused_rules))
        when (not (null unused_terminals)) $ hPutStrLn stderr ("unused terminals: " ++ show (length unused_terminals))
        return result

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

type ItemSetWithGotos = (Set Lr0Item, [(Name,Int)])
writeInfoFile :: [ItemSetWithGotos] -> Grammar -> ActionTable -> GotoTable -> Array Int (Int,Int) -> String -> Maybe String -> [Int] -> [String] -> IO ()
writeInfoFile sets g action goto conflictArray file info_file unused_rules unused_terminals = 
    let info = genInfoFile (map fst sets) g action goto (token_specs g) conflictArray file unused_rules unused_terminals in
        case info_file of
            Just s -> writeFile s info >> hPutStrLn stderr ("Grammar info written to: " ++ s)
            Nothing -> return ()