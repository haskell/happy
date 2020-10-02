module Follow where
  import Grammar
  import RADTools
  import Control.Monad
  import Control.Monad.ST
  import Data.Array.ST
  import GHC.Arr
  import Data.List (findIndices, tails)
  import NameSet (NameSet, empty, fromList, union, unions, delete, member, singleton)

  -- Calculate the follow sets for all nonterminals in the grammar.
  followArray :: Grammar -> ([Name] -> NameSet) -> Array Name NameSet
  followArray g first = runST $ do
    let bounds = liftM2 (,) head last (non_terminals g)
    arr <- newArray bounds empty
    startSymbols arr
    updateRepeatedly arr first
    elems <- getElems arr
    return (listArray bounds elems)
    where
      startSymbols :: (STArray s Int NameSet) -> ST s ()
      startSymbols arr = do
        mapM_ (setEOF arr) (starts g)
      setEOF :: (STArray s Int NameSet) -> (a, Int, b, c) -> ST s ()
      setEOF arr (_, nt, _, _) = writeArray arr nt (singleton (eof_term g))

      updateRepeatedly :: (STArray s Int NameSet) -> ([Name] -> NameSet) -> ST s ()      
      updateRepeatedly arr first = do
        old <- getElems arr
        updateStep arr first
        new <- getElems arr
        if old == new then return () else updateRepeatedly arr first
      
      updateStep :: (STArray s Int NameSet) -> ([Name] -> NameSet) -> ST s ()
      updateStep arr first = mapM_ (updateRule arr first) (productions g)
        
      updateRule :: (STArray s Int NameSet) -> ([Name] -> NameSet) -> Production -> ST s ()
      updateRule arr first (Production lhs rhs _ _) = mapM_ (updateNT arr lhs first) (tails rhs)
      
      updateNT :: (STArray s Int NameSet) -> Name -> ([Name] -> NameSet) -> [Name] -> ST s ()
      updateNT _ _ _ [] = return ()
      updateNT arr lhs first (tok:rhsRest)
        | not (elem tok (non_terminals g)) = return ()
        | otherwise = do
          let first' = first rhsRest
          let first'' = delete epsilonTok first'
          follow_lhs <- readArray arr lhs
          let new_follow = union first'' (if member epsilonTok first' then follow_lhs else empty)
          old_follow <- readArray arr tok
          writeArray arr tok (union old_follow new_follow)          

  -- The lc-follow set of a single nonterminal, given a full "follow" array.
  -- We only use rules where NT appears after the recognition point. If this is the case, enter into FOLLOW (not into LCFOLLOW),
  -- i.e. recursive rules are processed as normal, irrespective of their recognition points.
  lcfollow :: XGrammar -> ([Name] -> NameSet) -> (Array Name NameSet) -> Name -> NameSet
  lcfollow x@(XGrammar { g = g }) first follow nt
    | member nt startSymbols = union (singleton (eof_term g)) rest
    | otherwise = rest
    where
      startSymbols = fromList $ map (\(_, a, _, _) -> a) (starts g)
      
      rest = unions $ map (uncurry process) rules
      rules = filter (rhsContains nt) (zip [0..] (productions g))
      rhsContains nt (_, (Production _ rhs _ _)) = elem nt rhs

      process :: Int -> Production -> NameSet
      process ruleIndex (Production lhs rhs _ _) = unions $ map process' $ (reverse (findIndices (== nt) rhs)) where
        process' i
          | i < ((recognitionPoints x) !! ruleIndex) = empty
          | member epsilonTok first_b = union (delete epsilonTok first_b) (follow ! lhs)
          | otherwise = first_b
          where
          first_b = first (drop (i+1) rhs)