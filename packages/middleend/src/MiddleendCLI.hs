module MiddleendCLI(runMiddleend, MiddleendOpts(..)) where

import Middleend
import GenUtils
import FindRedundancies
import Info
import System.IO
import Grammar
import LALR
import Data.Array (Array)
import Data.Set (Set)

data MiddleendOpts = MiddleendOpts {
  inFile :: String, -- only used as information inside the info file
  infoFile :: Maybe String -- place where the info file should be written to
}

runMiddleend :: MiddleendOpts -> Grammar -> IO (ActionTable, GotoTable, [Lr1State], [Int])
runMiddleend opts g = 
   let first       = mkFirst g
       sets        = genLR0Items g
       la          = genLookaheads g sets first
       items2      = genLR1States la sets
       goto        = genGotoTable g sets
       action      = genActionTable g first items2
       (conflictArray, (sr,rr)) = (countConflicts action)
    in do
        (unused_rules, unused_terminals) <- reportUnusedRules g action
        writeInfoFile sets g action goto conflictArray (inFile opts) (infoFile opts) unused_rules unused_terminals
        reportConflicts g sr rr
        return (action, goto, items2, unused_rules)

reportUnusedRules :: Grammar -> ActionTable -> IO ([Int], [String])
reportUnusedRules g action = 
    let result@(unused_rules, unused_terminals) = find_redundancies first_reduction g action in do
        optIO (not (null unused_rules)) $ hPutStrLn stderr ("unused rules: " ++ show (length unused_rules))
        optIO (not (null unused_terminals))  $hPutStrLn stderr ("unused terminals: " ++ show (length unused_terminals))
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
writeInfoFile sets g action goto conflictArray file infoFile unused_rules unused_terminals = 
    let info = genInfoFile (map fst sets) g action goto (token_specs g) conflictArray file unused_rules unused_terminals in
        case infoFile of
            Just s -> writeFile s info >> hPutStrLn stderr ("Grammar info written to: " ++ s)
            Nothing -> return ()

optIO :: Bool -> IO a -> IO a
optIO fg io = if fg then io  else return (error "optIO")
