module RADTools where
  import Grammar
  import GHC.Arr
  import LALR
  import Data.Maybe
  import Data.List (elemIndex)
  import Data.IntSet ()
  import Data.IntMap ()
  import qualified Data.Set as Set
  import Data.Set (Set, toList, fromList, union, (\\))
  
  -- Lr1State as generated from happy
  type Lr1State = ([Lr1Item], [(Name, Int)])
    
  ----- COMPLETE LR0 STATES -----
  
  data CompletedLr0State = CompletedLr0State [Lr0Item] [Lr0Item]
      deriving (Eq
#ifdef DEBUG
      , Show
#endif
      )
    
  core :: CompletedLr0State -> [Lr0Item]
  core (CompletedLr0State a _) = a
  completion :: CompletedLr0State -> [Lr0Item]
  completion (CompletedLr0State _ a) = a

  -- Complete all states of the grammar, yielding a core and a completion set for each state.
  complete :: Grammar -> [Lr1State] -> [CompletedLr0State]
  complete g = map complete' where
    complete' state = CompletedLr0State core (completeWithFunction (directCompletion g) core) where
      core = (map lr0 $ fst state)
      lr0 (Lr1 rule dot _) = Lr0 rule dot
      
  -- Show a completed Lr0 state.
  showCompletedLr0State :: Grammar -> Int -> CompletedLr0State -> String
  showCompletedLr0State g i (CompletedLr0State core completion) =
    unlines $ ["State " ++ show i ++ ". CORE:"] ++ (map (showItem g) core) ++ ["COMPLETION:"] ++ (map (showItem g) completion)
  

  -- Perform a completion with a custom completion function.
  completeWithFunction :: (Lr0Item -> [Lr0Item]) -> [Lr0Item] -> [Lr0Item]
  completeWithFunction f core = toList $ complete' (fromList core) (fromList core) where
  
    -- Complete result. It is always newSet ⊆ result.
    complete' :: Set Lr0Item -> Set Lr0Item -> Set Lr0Item
    complete' result newSet
      | Set.null newSet = result
      | otherwise = complete' (union result newItems) (newItems \\ result) where
          newItems = join $ Set.map f newSet
          
    join :: Set [Lr0Item] -> Set Lr0Item
    join set = Set.fold (flip union . fromList) Set.empty set

  ----- LR0ITEM -----
    
  -- Get the number of tokens in the RHS of a rule.
  rhsLength :: Grammar -> Lr0Item -> Int
  rhsLength = rhsLength' .* prod
  rhsLength' :: Production -> Int
  rhsLength' (Production _ rhs _ _) = length rhs
  
  -- Get the production belonging to an item.
  prod :: Grammar -> Lr0Item -> Production
  prod g (Lr0 rule _) = lookupProdNo g rule
  
  -- Determine whether the dot is at the very right end of an item.
  dotIsAtRightEnd :: Grammar -> Lr0Item -> Bool
  dotIsAtRightEnd g item@(Lr0 _ dot) = (rhsLength g item) == dot
  
  -- Determine whether the dot is at the very right beginning of an item.
  dotIsAtLeftEnd :: Grammar -> Lr0Item -> Bool
  dotIsAtLeftEnd _ (Lr0 _ dot) = 0 == dot
  
  -- Determine if the dot is NOT at the very beginning of an item.
  -- Iff this returns true you may call tokenBeforeDot.
  hasTokenBeforeDot :: Grammar -> Lr0Item -> Bool
  hasTokenBeforeDot = not .* dotIsAtLeftEnd
  
  -- Determine if the dot is NOT at the very right end of an item.
  -- Iff this returns true you may call tokenAfterDot.
  hasTokenAfterDot :: Grammar -> Lr0Item -> Bool
  hasTokenAfterDot = not .* dotIsAtRightEnd
  
  -- Check whether a rule has a terminal symbol after its dot
  hasNonterminalAfterDot :: Grammar -> Lr0Item -> Bool
  hasNonterminalAfterDot g item = hasTokenAfterDot g item && isNonterminal g (tokenAfterDot g item)
  
  -- Get the token immediately after the dot.
  -- Diverges if the dot is at the end - call "hasTokenAfterDot" to determine if it is valid to call "tokenAfterDot"
  tokenAfterDot :: Grammar -> Lr0Item -> Name
  tokenAfterDot g item@(Lr0 _ dot) = tokenAtPosition g item dot

  -- Get the token immediately before the dot.
  -- Diverges if the dot is at the end - call "hasTokenBeforeDot" to determine if it is valid to call "tokenAfterDot"
  tokenBeforeDot :: Grammar -> Lr0Item -> Name
  tokenBeforeDot g item@(Lr0 _ dot) = tokenAtPosition g item (dot-1)
  
  -- Get the token in the RHS of an item at a certain position. The dot is ignored.
  tokenAtPosition :: Grammar -> Lr0Item -> Int -> Name
  tokenAtPosition g item pos = tokenAtPosition' (prod g item) pos where
    tokenAtPosition' (Production _ rhs _ _) = (rhs !!)
  
  -- Determine whether a token is a nonterminal
  isNonterminal :: Grammar -> Name -> Bool
  isNonterminal = flip elem . non_terminals
  
  -- Get the nonterminal on the left side of an item
  lhs :: Grammar -> Lr0Item -> Name
  lhs = lhs' .* prod
  lhs' :: Production -> Name
  lhs' (Production lhs _ _ _) = lhs
  
  rhsAfterDot :: Grammar -> Lr0Item -> [Name]
  rhsAfterDot g item@(Lr0 rule dot)
    | rule < 0 = if dot == 0 then [-rule] else [] -- artifical NT handling
    | otherwise = drop dot $ rhs (prod g item)

  rhs :: Production -> [Name]
  rhs (Production _ rhs _ _) = rhs
  
  -- Calculate the items which are in the IMEMDIATE completion of an item.
  -- For example, "A -> b . C D" has "C -> . D e", in its direct completion,
  -- but not the recursive completion "D -> . f" (which would be in the direct completion of "C -> . D e".
  -- The item itself may also be in its own completion.
  directCompletion :: Grammar -> Lr0Item -> [Lr0Item]
  directCompletion g item@(Lr0 rule dot)
    | rule < 0 && dot == 0 = itemsStartingWith g (-rule) -- special case: completion of artifical item _ -> . NT
    | hasNonterminalAfterDot g item = itemsStartingWith g (tokenAfterDot g item)
    | otherwise = []
    
  -- Determine whether item 2 is in the direct completion of item 1, as described above.
  -- Therefore, item 2 must be of the form "A -> (DOT) ..."
  -- An item may be in their own completion.
  isInDirectCompletion :: Grammar -> Lr0Item -> Lr0Item -> Bool
  isInDirectCompletion g item1@(Lr0 rule dot) item2@(Lr0 rule' dot')
    | rule' < 0 = False
    | rule < 0 = dot == 0 && dot' == 0 && itemStartsWith g item2 (-rule)
    | otherwise = 
      dot' == 0 &&
      hasNonterminalAfterDot g item1 &&
      itemStartsWith g item2 (tokenAfterDot g item1)
    
  -- Get all rules starting with "A -> ...", in form of the item "A -> (DOT) ..."
  itemsStartingWith :: Grammar -> Name -> [Lr0Item]
  itemsStartingWith g token = map toItem $ filter (startsWith token) $ (productions g) where
    toItem prod = Lr0 rule 0 where
      rule = fromJust $ elemIndex prod (productions g)
    startsWith token (Production token' _ _ _) = token == token'
    
  -- Determine whether the item starts with the token
  itemStartsWith :: Grammar -> Lr0Item -> Name -> Bool
  itemStartsWith g item token = startsWith token (prod g item) where
    startsWith token (Production token' _ _ _) = token == token'
  
  -- Convert an Lr0Item to a string, for example "A -> b · C D"
  showItem :: Grammar -> Lr0Item -> String
  showItem = showItemWithSeparator "·"

  showItemWithSeparator :: String -> Grammar -> Lr0Item -> String
  showItemWithSeparator sep g (Lr0 rule dot)
    | rule < 0 = -- artificial NT handling
      let nt = -rule in if dot == 0 then "_ -> " ++ sep ++ " " ++ showToken nt else "_ -> " ++ showToken nt ++ " " ++ sep
    
    | otherwise = showProd (lookupProdNo g rule) where
    showProd = unwords . showProdArray
    showProdArray (Production from to _ _) = insert sep (dot + 1) ([(showToken from) ++ " ->"] ++ (map showToken to))
    showToken tok = (token_names g) ! tok
    insert elem pos list = let (ys,zs) = splitAt pos list in ys ++ [elem] ++ zs
    
  ----- PRODUCTION -----
  
  -- Convert a production (represented by its index) to a string, for example "A -> b C D"
  showProd :: Grammar -> Int -> String
  showProd g i = unwords (showProdArray (lookupProdNo g i)) where
    showProdArray (Production from to _ _) = [(showToken from) ++ " ->"] ++ (map showToken to)
    showToken tok = (token_names g) ! tok

  -- Convert a production (represented by its index) and its recognition point to a string, for example "A -> b C . D"
  showRecognitionPoint :: XGrammar -> Int -> String
  showRecognitionPoint x rule = showItemWithSeparator "•" (g x) (Lr0 rule point) where
    point = (recognitionPoints x) !! rule

  ----- RAD-SPECIFIC -----
  
  -- Extended grammar containing RAD-relevant data like recognition points.
  data XGrammar = XGrammar {
    g :: Grammar,
    recognitionPoints :: [Int]
  }
  
  -- The rad-completion of a set of core items, defined as follows:
  -- Each core item is in the completion.
  -- If an item I = A -> b . C d is in the completion, and the dot is before the recognition point of the associated rule, then all items C -> ... are in the completion.
  radCompletion :: XGrammar -> [Lr0Item] -> [Lr0Item]
  radCompletion x core = completeWithFunction directRadCompletion core where
    directRadCompletion item@(Lr0 rule dot)
      | rule < 0 = directCompletion (g x) item -- special handling for item _ -> . NT
      | dot < (recognitionPoints x) !! rule = directCompletion (g x) item
      | otherwise = []
  

  -- Perform Q+'X, but only consider items in Q/'X (i.e. where the dot is before the recognition point):
  -- Q+'X = { A -> β X . Ɣ | A -> β . X Ɣ elem Q, recog. point is after X }
  plusRad :: [Lr0Item] -> Name -> XGrammar -> [Lr0Item]
  plusRad q y x = filter nonready' (plus q y (g x)) where
    nonready' (Lr0 rule dot) = dot <= (recognitionPoints x) !! rule

  -- Perform Q/X, but discard items where the dot is at or after the recognition point:
  -- Q/'X = { A -> β . X Ɣ elem Q, recog. point is after X }
  hdivRad :: [Lr0Item] -> Name -> XGrammar -> [Lr0Item]
  hdivRad q y x = filter nonready (hdiv q y (g x)) where
    nonready (Lr0 rule dot) = dot < (recognitionPoints x) !! rule
  
  -- Perform Q+X as described by Hinze:
  -- Q+X = { A -> β X . Ɣ | A -> β . X Ɣ elem Q }
  plus :: [Lr0Item] -> Name -> Grammar -> [Lr0Item]
  plus q x g = map shiftDot (hdiv q x g) where
    shiftDot (Lr0 rule dot) = Lr0 rule (dot+1)
    
  -- Perform Q/X as described by Hinze:
  -- Q/X = { A -> β . X Ɣ elem Q }
  hdiv :: [Lr0Item] -> Name -> Grammar -> [Lr0Item]
  hdiv q x g = filter matches q where
    matches item = hasTokenAfterDot g item && tokenAfterDot g item == x

  ----- MORE -----

  (.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
  (.*) = (.) . (.)