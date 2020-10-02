module RADStateGen (generateLALRStates, generateRADStates, createXGrammar, artCore, hdiv, plus, RADType(..), RADState(..), LALRState(..), RawRADState(..), LALRDefaultAction(..), RADDefaultAction(..)) where
  import Grammar
  import Data.Graph.Dom
  import First
  import Follow
  import NameSet (NameSet)
  import Data.Graph
  import Data.Set (Set, toList, fromList, elemAt)
  import qualified Data.IntSet
  import LALR
  import RADTools (CompletedLr0State, Lr1State, XGrammar(..), complete, showItem, showProd, lhs, core, completion, prod, hasTokenAfterDot, tokenAfterDot, rhsLength', isInDirectCompletion, dotIsAtRightEnd, plus, hdiv, radCompletion, itemsStartingWith, plusRad, completeWithFunction, directCompletion, rhsAfterDot)
  import Control.Monad
  import Data.List
  import Data.Maybe
  import Data.Ord
  import Data.Function (on)
  import GHC.Arr ((!), assocs, listArray, Array(..))
  
  
  -- Types which are used (both) for LALR and RAD states:
  type AcceptAction = (Name)                    -- On terminal t --> accept
  type AnnounceAction = (Name, Int)             -- On terminal t --> announce using rule i
  type ReduceAction = (Name, Int)               -- On terminal t --> reduce using rule i
  type ShiftAction = (Name, (Int, [Lr0Item]))   -- On terminal t --> goto state S with items I (= A -> B t . C)
  type GotoAction = (Name, (Int, [Lr0Item]))    -- Nonterminal X <-> goto to state S with items I (= A -> B X . C) such that:
                                                -- g_X v = state_{i+X} (k_i v)
  
  -------------------- RAD STATE --------------------
  
  data RADType = Type1 -- States with the item _ -> |- . NT (here a top-down parse is started)
               | Type2 -- States with the item _ -> |- NT . (here a top-down parse is accepted)
               | Type3 -- Normal states with no artificial item
               deriving (Show, Eq)
  
  data RawRADState = RawRADState {
    i :: Int, -- The final index that the completed RADState state will also have
    comingFrom :: Int,
    radType :: RADType,
    state :: LALRState,
    nt :: Name, -- The NT which is used by Type1 or Type2 states. Attention: undefined for Type3 states
    core' :: [Lr0Item],
    completion' :: [Lr0Item] -- contains core
  } deriving (Eq
#ifdef DEBUG
    , Show
#endif
    )
  
  createType1State :: XGrammar -> Name -> LALRState -> Int -> Int -> RawRADState
  createType1State x nt state i comingFrom = RawRADState { i = i, radType = Type1, state = state, nt = nt, core' = [], completion' = radCompletion x (itemsStartingWith (g x) nt), comingFrom = comingFrom } -- has artifical core item _ -> |- . NT
  
  createType2State :: XGrammar -> Name -> LALRState -> [Lr0Item] -> Int -> Int -> RawRADState
  createType2State x nt state core i comingFrom = RawRADState { i = i, radType = Type2, state = state, nt = nt, core' = core, completion' = radCompletion x core, comingFrom = comingFrom } -- has artifical core item _ -> |- NT .
  
  createType3State :: XGrammar -> LALRState -> [Lr0Item] -> Int -> Int -> RawRADState
  createType3State x state core i comingFrom = RawRADState { i = i, radType = Type3, state = state, nt = undefined, core' = core, completion' = radCompletion x core, comingFrom = comingFrom }

  data RADState = RADState {
    announces' :: [AnnounceAction],
    gotos' :: [GotoAction],
    shifts' :: [ShiftAction],
    accepts' :: [AcceptAction], -- ONLY type2 states can have (and always do have) accept actions!
    defaultAction' :: RADDefaultAction,
    _raw :: RawRADState
  } deriving (Eq
#ifdef DEBUG
    , Show
#endif
    )
  
  data RADDefaultAction = ErrorShift' Int -- On errorToken (i.e. default), shift to state X
                        | Announce' Int -- Announce rule X
                        | Accept' -- Accept the NT. Only in type2-states
                        | Error' -- Call happyError
                        deriving (Eq, Show)
  
  -- The core PLUS the possible artificial item.
  -- Artificial items have negative rule numbers - they look like this: "Lr0 (-4) 0" for the item "|- -> . (NT4)".
  artCore :: RADState -> [Lr0Item]
  artCore state = case (radType . _raw $ state) of
    Type1 -> (Lr0 (-nt') 0) : core
    Type2 -> (Lr0 (-nt') 1) : core
    Type3 -> core
    where
      core = core' (_raw state)
      nt' = nt (_raw state)

  showRadState :: XGrammar -> RADState -> [Char]
  showRadState x state = "Raw = " ++ showRaw x (_raw state) ++ "\nShifts = " ++ show (shifts' state) ++ "\nGotos = " ++ show (gotos' state) ++ "\nAnnounces = " ++ show (announces' state) ++ "\nAccepts = " ++ show (accepts' state) ++ "\nDefault = " ++ show (defaultAction' state) ++ "\n\n"
  
  showRaw :: XGrammar -> RawRADState -> [Char]
  showRaw x raw = "RawRADState " ++ show (i raw) ++ ": " ++ show (radType raw) ++ " (orig state: " ++ show (index (state raw)) ++ " " ++ show (map (showItem (g x)) (coreItems (state raw))) ++ ")" ++
    (if radType raw /= Type3 then ". NT = " ++ show (nt raw) ++ " (" ++ ((token_names (g x)) ! (nt raw)) ++ ")" else "") ++
    (if radType raw /= Type1 then ". core = { " ++ intercalate "; " (map (showItem (g x)) (core' raw)) ++ " }" else "") ++
    ". completion = { " ++ intercalate "; " (map (showItem (g x)) (completion' raw)) ++ " }" ++
    " (Coming from state " ++ show (comingFrom raw) ++ ")"


  -- Create the extended grammar containing information about the recognition points.
  createXGrammar :: Grammar -> [LALRState] -> IO XGrammar
  createXGrammar g lalrStates = do
    -- Create state graphs; determine recognition points for each rule
    let allGraphs = map (recognitionGraph g) lalrStates
    let nonfree = nonfreeItems g allGraphs
    let recognitionPoints = determineRecognitionPoints g nonfree
    
    let x = XGrammar { g = g, recognitionPoints = recognitionPoints }
    
#ifdef DEBUG
    debugPrint "State Graphs:" (showGraph g) allGraphs
    --debugPrint "Non-Free Items:" (showItem g) nonfree
    debugPrint "All Rules With Their Recognition Points:" (showRecognitionPoint x) [0 .. (length (productions g)) - 1]
#endif
    
    return x
    
  -- Generate all RAD states from happy's LALR states.
  generateRADStates :: XGrammar -> [LALRState] -> [Int] -> IO [RADState]
  generateRADStates x lalrStates unusedRules = do
    let g = RADTools.g x
    let first = mkFirst g
    let follow = followArray g first
    let radStates = lalrToRADStates x lalrStates unusedRules first follow
    
#ifdef DEBUG
    -- debugPrint "LALRStates:" (showState g) lalrStates
    debugPrint "RADStates:" (showRadState x) radStates
#endif
    
    return radStates
    
  -- Helper function for printing.
  
  debugPrint :: String -> (a -> String) -> [a] -> IO ()
  debugPrint title showElem elems = putStrLn $ break ++ dash ++ "\n" ++ title ++ break ++ unlines (map showElem elems) ++ dash ++ break where
    dash = replicate 40 '–'
    break = "\n\n"

  -- Convert all LALR states to RAD states.
  lalrToRADStates :: XGrammar -> [LALRState] -> [Int] -> ([Name] -> NameSet) -> Array Name NameSet -> [RADState]
  lalrToRADStates x@(XGrammar { g = g, recognitionPoints = recognitionPoints }) lalrStates unusedRules first follow = gen' x [] rawType1States where
    rawType1States = map (uncurry toType1) (zip unambiguousNTs [0..])
    
    -- Unambiguous NTs are NTs that appear after the recognition point in some rule
    unambiguousNTs = filter hasGoodRule (non_terminals g)
    hasGoodRule = not . null . findGoodRule
    findGoodRule nt = find (uncurry ntAppearsAfterRecogPoint) (zip [0..] (productions g)) where
      ntAppearsAfterRecogPoint i (Production _ rhs _ _) = elem nt (drop (recognitionPoints !! i) rhs) && notElem i unusedRules

    -- Find a state with a (completion) item where the dot is immediately before the NT; create a type1-state
    toType1 nt index = createType1State x nt state index (-1) where
      (i, (Production _ rhs _ _)) = fromJust $ findGoodRule nt
      posBeforeNT = (length rhs - 1) - (fromJust $ findIndex (nt ==) (reverse rhs))
      item = Lr0 i posBeforeNT
      state = fromJust $ find (elem item . completionItems) lalrStates
  
    -- Complete the raw states to full RADStates, possibly yielding new raw states which will be recursively completed.
    gen' :: XGrammar -> [RADState] -> [RawRADState] -> [RADState]
    gen' _ states [] = states
    gen' x states rs@(raw:raws) = gen' x (states ++ [fresh]) (raws ++ new) where
      (fresh, new) = completeRaw x lalrStates raw existingRaws first follow (length states + length rs)
      existingRaws = raws ++ map _raw states
  

  -- Complete a raw state to a RADState, possibly yielding new raw states.
  -- The "new" raw states which are created for shifting/goto can also be existing ones; therefore, the list of all already created raw states is passed around.
  completeRaw :: XGrammar -> [LALRState] -> RawRADState -> [RawRADState] -> ([Name] -> NameSet) -> Array Name NameSet -> Int -> (RADState, [RawRADState])
  completeRaw x@(XGrammar { g = g }) allStates raw allRawStates first follow stateCount = (radState, newStates) where
    radState = RADState { shifts' = shifts', accepts' = accepts'', announces' = announces'', gotos' = gotos', defaultAction' = default'', _raw = raw }
    newStates = gotoStates ++ shiftStates ++ (maybe [] return newStateFromTransformedErrorShift)
    gotos' = transformedGotos
    shifts' = shiftShifts
    
    announces' = announcesFromReduces ++ shiftAnnounces ++ type1EpsilonAnnounces
    announces'' = case default'' of -- If default action is announce, remove unnecessary entries
      Announce' rule -> filter ((/=) rule . snd) $ filter ((/=) errorTok . fst) announces'
      _ -> filter ((/=) errorTok . fst) announces'
      
    accepts' = shiftAccepts ++ type2Accepts
    accepts'' -- If default action is accept, no need for an accept array
      | default'' == Accept' = []
      | otherwise = delete errorTok (rmdups accepts') where rmdups = map head . group . sort
      
    -- If there is no transformed default action, we choose a suitable default action:
    -- Accept for type2 states, or the largest announce action for other states.
    -- If there is an accept or announce action on the errorToken, use this as the default action.
    default''
      | elem errorTok accepts' = if default' == Error' then Accept' else error ("errorTok is in AcceptActions, but defaultAction is " ++ (show default')) -- check for accept conflict, shouldn't happen
      | any ((==) errorTok . fst) announces' = case default' of
        Error' -> Announce' defaultRule
        Announce' rule -> if rule == defaultRule then Announce' defaultRule else
             error $ "errorTok wants to announce rule " ++ show defaultRule ++ ", but defaultAction is " ++ (show default')
        _ -> error $ "errorTok wants to announce rule " ++ show defaultRule ++ ", but defaultAction is " ++ (show default')
      | default' /= Error' = default' -- Keep transformed default action 
      | radType raw == Type2 = Accept' -- Type2 states accept per default
      | not (null announces') = Announce' largestAnnounce
      | otherwise = Error'
      where
        defaultRule = snd $ fromJust $ find ((==) errorTok . fst) announces'
        largestAnnounce = head . head $ sortBy (flip (comparing length)) (group (sort (map snd announces')))
        
    -- Transform the LALR default action
    default' = fromMaybe Error' (transformDefault (defaultAction (state raw)))
    transformDefault Error = Just Error'
    transformDefault (Reduce rule) = do
      (_, rule') <- transformReduce (errorTok, rule)
      return $ Announce' rule'
    
    transformDefault (ErrorShift _) = case (fromJust transformedErrorShift) of
        (Just (_, (state, _)), _, _, _) -> Just (ErrorShift' state)
        (_, _, Just (_, rule), _) -> Just (Announce' rule)
        _ -> Nothing -- The default action could be irrelevant for the RAD state
    
    transformedErrorShift = case (defaultAction (state raw)) of
      ErrorShift state -> Just $ transformShift stateNum (errorTok, (state, undefined)) where stateNum = stateCount + length (gotoStates ++ shiftStates)
      _ -> Nothing
    newStateFromTransformedErrorShift = maybe Nothing (\(_, s, _, _) -> s) transformedErrorShift
    
    -- Goto actions and new goto-states:
    (transformedGotos, gotoStates) = (catMaybes transformedGotos', catMaybes gotoStates')
    (transformedGotos', gotoStates') = unzip $ allGotos' stateCount (gotos (state raw)) [] where
      allGotos' :: Int -> [GotoAction] -> [(Maybe GotoAction, Maybe RawRADState)] -> [(Maybe GotoAction, Maybe RawRADState)]
      allGotos' _ [] result = result
      allGotos' nextIndex (goto:gotos) result = case transformGoto nextIndex goto of
        res@(_, Just _) -> allGotos' (nextIndex + 1) gotos (result ++ [res])
        res@(_, Nothing) -> allGotos' nextIndex gotos (result ++ [res])
    
    -- Transform a normal goto-action into a RAD-goto action to a new type3-state.
    -- This function creates both the goto action and the new state.
    -- Return Nothing if the goto is not required for RAD.
    -- Type-1-specific: The artificial item '_ -> |- . NT' yields a goto action to a new type2(!)-state.
    transformGoto :: Int -> GotoAction -> (Maybe GotoAction, Maybe RawRADState)
    transformGoto index (tok, (gotoState, _))
      | not isGotoFromType1ToType2 && null gotoItems = (Nothing, Nothing)
      | otherwise = (Just newAction, newState)
      where
        isGotoFromType1ToType2 = radType raw == Type1 && tok == (nt raw) -- type1-specific. May have no goto items as an articifial item is created
        gotoItems = plusRad (completion' raw) tok x
          
        newState
          | isGotoFromType1ToType2 = Just $ createType2State x tok (allStates !! gotoState) gotoItems index (i raw)
          | null existingState = Just $ createType3State x (allStates !! gotoState) gotoItems index (i raw)
          | otherwise = Nothing
          
        newAction = (tok, (index, allItems)) where
          allItems = if isGotoFromType1ToType2 then artificial:gotoItems else gotoItems -- gotoItems + artificial item
          artificial = Lr0 (-(nt raw)) 1
          index = maybe (i $ fromJust newState) i existingState
        
        -- an existing (type3) state with the same core can be reused, if existing
        existingState
          | isGotoFromType1ToType2 = Nothing
          | otherwise = find matchesState allRawStates
          where
            matchesState raw' = gotoItems == core' raw' && radType raw' == Type3


    -- Transform shift actions into shift, announce and accept actions:
    (shiftShifts, shiftStates, shiftAnnounces, shiftAccepts) = (catMaybes shiftShifts', catMaybes shiftStates', catMaybes shiftAnnounces', catMaybes shiftAccepts')
    (shiftShifts', shiftStates', shiftAnnounces', shiftAccepts') = unzip4 $ allShifts' (stateCount + length gotoStates) (shifts (state raw)) [] where
      allShifts' :: Int -> [ShiftAction] -> [(Maybe ShiftAction, Maybe RawRADState, Maybe AnnounceAction, Maybe AcceptAction)] -> [(Maybe ShiftAction, Maybe RawRADState, Maybe AnnounceAction, Maybe AcceptAction)]
      allShifts' _ [] result = result
      allShifts' nextIndex (shift:shifts) result = case transformShift nextIndex shift of
        res@(_, Just _, _, _) -> allShifts' (nextIndex + 1) shifts (result ++ [res])
        res@(_, Nothing, _, _) -> allShifts' nextIndex shifts (result ++ [res])
      
    -- Transform a normal shift-action into one of the following:
    -- A shift action (with a new state), an announce action or an accept action, or nothing if the shift is not relevant for the RAD state.
    transformShift :: Int -> ShiftAction -> (Maybe ShiftAction, Maybe RawRADState, Maybe AnnounceAction, Maybe AcceptAction)
    transformShift index (tok, (shiftState, _))
      | not (null gotoItems) = (Just shift, newState, Nothing, Nothing)
      | not (null announcedRule) = (Nothing, Nothing, Just (tok, rule'), Nothing)
      | radType raw == Type2 = (Nothing, Nothing, Nothing, Just tok) -- TODO: sinnvoll?
      | otherwise = (Nothing, Nothing, Nothing, Nothing)
      where
        gotoItems = plusRad (completion' raw) tok x
        shift = (tok, (index, gotoItems)) where
          index = maybe (i $ fromJust newState) i existingState
        
        -- an existing (type3) state with the same core can be reused, if existing
        existingState = find matchesState allRawStates where
          matchesState raw' = gotoItems == core' raw' && radType raw' == Type3
          
        newState = case existingState of
          Just _ -> Nothing
          Nothing -> Just $ createType3State x (allStates !! shiftState) gotoItems index (i raw)
          
        announcedRule = getAnnouncedRule tok
        Just rule' = announcedRule

    -- Announce actions from reduce actions:
    announcesFromReduces = mapMaybe transformReduce (reduces (state raw))
    -- Transform a normal reduce-action into a RAD-announce action.
    -- Return Nothing if the announce is not relevant for the RAD state.
    transformReduce :: ReduceAction -> Maybe AnnounceAction
    transformReduce (tok, rule)
      | elem (Lr0 rule veryRight) (completion' raw) = Just (tok, rule)
      | not (null announcedRule) = Just (tok, rule')
      | otherwise = Nothing
      where
        veryRight = rhsLength' (lookupProdNo g rule)
        announcedRule = getAnnouncedRule (lhs g (Lr0 rule 0))
        Just rule' = announcedRule
        
    -- Accept actions for type-2 states:
    -- When a token of the lc-follow-set of NT (on which we accept NT) already has another action, we get an accept conflict.
    type2Accepts
      | (radType raw) == Type2 = catMaybes $ map toAccept (Data.IntSet.toList $ lcfollow x first follow (nt raw))
      | otherwise = []
      where
        toAccept tok
          | tok == 0 = Nothing -- epsilon ∈ follow(NT)
          | hasOtherAction tok = Nothing -- No accept conflict! This happens e.g. on shift/reduce-conflicts which have been resolved in favor of shift 
          | otherwise = Just tok
        hasOtherAction tok = elem tok otherActions
        otherActions = (map fst shifts') ++ (map fst announces')
        
    -- For a type-1 action: If NT can produce ɛ (either directly, NT -> ɛ or indirectly, NT -> A so that A ->* ɛ) we need a special announce action to announce a related rule.
    type1EpsilonAnnounces
      | (radType raw) == Type1 && canProduceEpsilon [nt raw] = map toAnnounce validTokens
      | otherwise = []
      where
        -- All tokens that are in the lc-follow set will produce the special announce action.
        validTokens = filter isCandidate allCandidates where
          isCandidate tok = tok /= epsilonTok && not (alreadyHasAction tok)
          allCandidates = Data.IntSet.toList $ lcfollow x first follow (nt raw)
          alreadyHasAction tok -- = elem tok announces || elem tok otherActions where
            | elem tok announces = True
            | elem tok otherActions = True -- seq (unsafePerformIO (print $ "token " ++ show tok ++ "already has other action, ignoring")) True
            | otherwise = False
            where
            announces = map fst (announcesFromReduces ++ shiftAnnounces)
            otherActions = (map fst shifts') ++ (map id accepts')
            
        toAnnounce tok = (tok, announcedRule)
        
        -- The question whether the right hand side of a rule can produce epsilon.
        canProduceEpsilon = Data.IntSet.member epsilonTok . first
        
        -- Create the graph consisting of all items in the NT's completion which CAN PRODUCE EPSILON.
        -- From these, there should be a way from the NT to a leaf node (X -> .)
        core = [Lr0 (-(nt raw)) 0]
        
        reducedCompletion = filter itemCanProduceEpsilon (completeWithFunction (directCompletion g) core) where
          itemCanProduceEpsilon = canProduceEpsilon . rhsAfterDot g
          
        (_, _rooted, nodes) = recognitionGraph g artState where
          artState = LALRState { index = 0, coreItems = core, completionItems = reducedCompletion, shifts = [], gotos = [], reduces = [], defaultAction = Error }
        graph = convert _rooted
        
        -- All vertices reachable from the root node
        connectedVertices = delete 0 (reachable graph 0) 
        
        -- Find all reachable leaf vertices of the form X -> .
        -- Optimally, there should only be a single one of these.
        epsilonItems = filter (isEpsilon . (!!) nodes) connectedVertices where
          isEpsilon (Item (Lr0 rule _) _) = rule >= 0 && let (Production _ rhs _ _) = (lookupProdNo g rule) in null rhs
          isEpsilon _ = False
        
        leafNode = case epsilonItems of
          [] -> error $ "Cannot happen - there must be an item of the form X -> . in the completion of NT " ++ show ((token_names g) ! (nt raw))
          [item] -> item
          _ -> error $ "Multiple leaf nodes X -> . in the completion of NT " ++ show ((token_names g) ! (nt raw))
        
        -- All cycle-free paths between 1 (the node _ -> |- . NT) and the leaf node.
        allPaths = connect 1 leafNode graph
        
        connect x y g = helper x y g [x] where -- all cycle-free paths between x and y, from https://stackoverflow.com/questions/11168238
          helper a b g visited
            | a == b    = [[]]
            | otherwise = [(a,c):path | c <- g!a, c `notElem` visited, path <- helper c b g (c:visited)]
        
        -- Get the single path from the root node 1 to the leaf node
        path = case allPaths of
          [] -> error $ "Should not happen - there must be a path from the root node to the epsilon node"
          [path] -> path
          _ -> error $ "Multiple paths from the root node (" ++ showNode (nodes !! 1) ++ ") to epsilon node (" ++ showNode (nodes !! leafNode) ++ ")" where
        showNode (Item item _) = showItem g item
        showNode _ = ""
            
        -- Find the item / rule to be announced.
        -- It is any rule on the path which both:
        --  - has the recognition point at the beginning and
        --  - is in the rad-completion of the current raw-LC-state.
        elements = map snd path -- discard the root node (it cannot be announced as it doesn't correspond to any rule)
        validElements = filter (valid . (!!) nodes) elements where
          valid (Item item@(Lr0 rule _) _) = (recognitionPoints x) !! rule == 0 && elem item (completion' raw)
          valid _ = False
          
        announcedItem = case validElements of
          [] -> error $ "No valid rule to be announced for epsilon-production " ++ showNode (nodes !! leafNode) ++ "in item " ++ showNode (nodes !! 1) 
          items -> head items
          
        announcedRule = let (Item (Lr0 rule _) _) = nodes !! announcedItem in rule

    -- This is the traceback of a shift or reduce action to the item and rule through whose recursive completion it was added to the RAD state's completion.
    getAnnouncedRule :: Name -> Maybe Int
    getAnnouncedRule t = recursiveAnnouncedRule t [] where
      -- This version takes a list of inputs that directly return Nothing (i.e. will recurse) to avoid infinte recursion: going in a recursion cycle will return Nothing.
      recursiveAnnouncedRule :: Name -> [Name] -> Maybe Int
      recursiveAnnouncedRule token nulls
        | elem token nulls = Nothing
        | otherwise = case directRule of
        Just rule -> elemIndex (prod g rule) (productions g)
        Nothing -> extendedRule
        where
        
        directRule = find matchingReadyRule (completion' raw)
        matchingReadyRule item@(Lr0 rule dot) = (recognitionPoints x) !! rule == dot && hasTokenAfterDot g item && tokenAfterDot g item == token
        extendedRule = case length extendedRules of
          0 -> Nothing
          1 -> Just (elemAt 0 extendedRules)
          _ -> error $ "Announce conflict! Possible rules: { " ++ intercalate "; " (map (showProd g) (toList extendedRules)) ++ " } in RAD-State " ++ showRaw x raw ++ "!"
        extendedRules :: Set Int
        extendedRules = mapMaybeSet recursive (fromList $ completionItems (state raw))
        recursive item@(Lr0 _ dot')
          | dot' == 0 && hasTokenAfterDot g item && tokenAfterDot g item == token = recursiveAnnouncedRule (lhs g item) (token:nulls) -- avoids infinite recursion
          | otherwise = Nothing
        
        mapMaybeSet :: Ord b => (a -> Maybe b) -> Data.Set.Set a -> Data.Set.Set b
        mapMaybeSet f = Data.Set.fromList . Data.Maybe.mapMaybe f . Data.Set.toList
    

 -------------------- LALR STATE GENERATION --------------------

  -- Create LALRStates from happy's action and goto tables, bundling state information in a single datatype.
  generateLALRStates :: Grammar -> ActionTable -> GotoTable -> [Lr1State] -> [LALRState]
  generateLALRStates g action goto happystates = do
    let completed = complete g happystates
    let numbered = zip [0..] completed
    let lalrStates = map (uncurry $ createState g action goto) numbered
    lalrStates
  
  -- `State` bundles required symbol-item mappings for creating a Hinze-like continuation-based
  -- state function for a state.
  -- It combines the data from goto and action tables in one coherent data structure.
  data LALRState = LALRState {
    index :: Int,
    coreItems :: [Lr0Item],
    completionItems :: [Lr0Item],
    shifts :: [ShiftAction],
    gotos :: [GotoAction],
    reduces :: [ReduceAction],
    defaultAction :: LALRDefaultAction -- Do something per default. This action is NOT explicitly mentioned in the shifts/reduces list.
  } deriving (Eq
#ifdef DEBUG
    , Show
#endif
    )
  
  data LALRDefaultAction = ErrorShift Int -- On errorToken (i.e. default), shift to state X
                         | Reduce Int -- Reduce rule X
                         | Error -- Call happyError
                         deriving (Eq, Show)
  
  showState :: Grammar -> LALRState -> [Char]
  showState g state = "State " ++ show (index state) ++
    " { " ++ unwords (map (showItem g) (coreItems state)) ++ " }" ++
    " – Shifts: " ++ unwords (map showShift (shifts state)) ++
    " – Gotos: " ++ unwords (map showGoto (gotos state)) ++
    " – Reduces: " ++ unwords (map showReduce (reduces state)) ++
    " – Default: " ++ show (defaultAction state) where
      showShift (token, (i, items)) = "(on " ++ (token_names g)!token ++ " shift to " ++ show i ++ " with items: " ++ unwords (map (showItem g) items) ++ ")"
      showGoto (nt, (i, items)) = "(on " ++ (token_names g)!nt ++ " goto " ++ show i ++ " with items: " ++ unwords (map (showItem g) items) ++ ")"
      showReduce (token, rule) = "(on " ++ (token_names g)!token ++ " reduce rule " ++ show rule ++ ")"
  
  -- Create `State` data from the given `CompletedLr0State` and its index.
  createState :: Grammar -> ActionTable -> GotoTable -> Int -> CompletedLr0State -> LALRState
  createState g action goto i state = LALRState { index = i, coreItems = core state, completionItems = completion state, shifts = shifts, gotos = gotos, reduces = reduces, defaultAction = defaultAction } where
    gotos = map toGoto allGotos
    toGoto (nt, Goto toState) = (nt, (toState, items)) where
      items = plus (completion state) nt g
    allGotos = filter isGoto $ assocs (goto ! i)
    isGoto (_, Goto {}) = True
    isGoto _ = False
    
    shifts' = map toShift allShifts
    toShift (token, LR'Shift toState _) = (token, (toState, shiftItems token))
    toShift (token, LR'Multiple _ (LR'Shift toState _)) = (token, (toState, shiftItems token))
    shiftItems token = plus (completion state) token g
    allShifts = filter isShift $ assocs (action ! i) -- all shifts from action table
    isShift (_, LR'Shift {}) = True
    isShift (_, LR'Multiple _ (LR'Shift {})) = True
    isShift _ = False
    
    reduces' = map toReduce allReduces
    toReduce (token, LR'Reduce rule _) = (token, rule)
    toReduce (token, LR'Multiple _ (LR'Reduce rule _)) = (token, rule)
    toReduce (token, LR'Accept) = let (Lr0 rule _) = head (core state) in (token, rule)
    toReduce (token, LR'Multiple _ LR'Accept) = let (Lr0 rule _) = head (core state) in (token, rule)
    allReduces = filter isReduce $ assocs (action ! i) -- all reduces from action table
    isReduce (_, LR'Reduce {}) = True
    isReduce (_, LR'Multiple _ (LR'Reduce {})) = True
    isReduce (_, LR'Accept) = True
    isReduce (_, LR'Multiple _ LR'Accept) = True
    isReduce _ = False
    
    -- Remove default action (errorShift or reduce) from shifts/reduces
    shifts = filter (\(token, _) -> token /= errorTok) shifts'
    reduces = filter test reduces' where
      test = if defaultReduce then (\(_, rule) -> rule /= defaultReduceRule) else return True
    
    defaultErrorShift = any isErrorAction shifts' where
    errorShiftState = (fst . snd . fromJust) (find isErrorAction shifts')
    
    isErrorAction (token, _) = token == errorTok
      
    defaultReduce = not defaultErrorShift && not (null reduces')
    defaultReduceRule = fromMaybe largestRule errorRule where
      errorRule = (find isErrorAction reduces') >>= Just . snd
      
      largestRule = (snd . head . head) sortedGroups -- Find reduce rule which is used most often (i.e. by most tokens)
      sorted = sortBy (comparing snd) reduces'
      grouped = groupBy ((==) `on` snd) sorted
      sortedGroups = sortBy (flip (comparing length)) grouped
    
    defaultAction
      | defaultErrorShift = ErrorShift errorShiftState
      | defaultReduce = Reduce defaultReduceRule
      | otherwise = Error



  -------------------- DETERMINING RECOGNITION POINTS --------------------
  
  -- Determine the recognition points for each rule from the set of all non-free items.
  determineRecognitionPoints :: Grammar -> [Lr0Item] -> [Int]
  determineRecognitionPoints g nonfree = map (uncurry recognitionPoint) (zip [0..] (productions g)) where
    
    -- No priority -> recognition point = first position where all consecutive positions are free
    recognitionPoint rule (Production _ rhs _ No) = maybe 0 (+1) $ find isNonfree (reverse [0 .. length rhs-1]) where 
      isNonfree i = elem (Lr0 rule i) nonfree || (rhs !! i) == errorTok -- recognition point must come after all error tokens
      
    -- Priority/associativity -> recognition point must be at the very right
    recognitionPoint _ (Production _ rhs _ _) = length rhs
  
  -- Determine all non-free items from all state graphs.
  nonfreeItems :: Grammar -> [RecognitionGraph] -> [Lr0Item]
  nonfreeItems _ graphs = (toList . fromList . join) nonfrees where -- removing duplicates
    nonfrees = map (\((_, g, v), i) -> nonfree g i v) $ zip graphs [0..]
    nonfree g i nodes = map lr0 $ filter (not . isFree) [0 .. numNodes g-1] where
      dom' = dom g
      isFree v = all (dominates dom' v) reachableLeafs where
        reachableLeafs = filter (isLeaf . (!!) nodes) (reach g v)
        isLeaf (ShiftNode _ _) = True; isLeaf (ReduceNode _ _) = True; isLeaf _ = False
      lr0 = lr0' . (!!) nodes where lr0' (Item a _) = a
      
  -- The number of nodes of a rooted graph.
  numNodes :: Rooted -> Int
  numNodes = length . toAdj . snd

  -- Convert a Rooted (used for domination) to a Data.Graph.Graph (used for reachability)
  convert :: Rooted -> Data.Graph.Graph
  convert g = listArray (0, numNodes g-1) (map snd (toAdj (snd g)))
  
  -- All nodes that can be reached from this node using at least 1 edge.
  -- This means a node only reaches itself it participates in a cycle.
  reach :: Rooted -> Vertex -> [Vertex]
  reach = reach' . convert where
    reach' g v = reachWithoutV ++ (if isCycle then [v] else []) where
      reachWithoutV = delete v (reachable g v)
      isCycle = elem v (g ! v) || any reachesV reachWithoutV
      reachesV w = elem v (reachable g w)
    
  -- True if a dominates b. A node never dominates itself.
  dominates :: [(Node, Path)] -> Node -> Node -> Bool
  dominates dom' a b = contains a pair where
    pair = find ((b ==) . fst) dom'
    contains a = maybe False (elem a . snd)
  

  -------------------- RECOGNITION GRAPH CREATION --------------------
    
  data RecognitionNode = Init
                        | Item Lr0Item Bool -- item, isCore
                        | ShiftNode Int Name -- shift to state; token which triggers the shift
                        | ReduceNode Int (Maybe Name) -- rule which is reduced; token which triggers the reduce (can be Nothing -> it is a default reduce)
                        deriving (Eq
#ifdef DEBUG
                        , Show
#endif
                        )
  type RecognitionGraph = (Int,               -- Rule number.
                           Rooted,            -- Rooted uses Ints to decode the nodes,
                           [RecognitionNode]) -- so this is the ordered list of all nodes
                        
  -- Create the rooted state graph for a state which is in turn used to determine the recognition points.
  recognitionGraph :: Grammar -> LALRState -> RecognitionGraph
  recognitionGraph g state@(LALRState { index = i, coreItems = core, completionItems = completion, shifts = shifts, reduces = reduces, defaultAction = defaultAction }) = graph i allNodes succ where
    
    -- All nodes of the graph
    allNodes = [initNode] ++ coreNodes ++ completionNodes ++ shiftNodes ++ reduceNodes ++ defaultNode where
      initNode = Init
      coreNodes = map (flip Item True) core
      completionNodes = map (flip Item False) (filter (not . (flip elem) core) completion)
      shiftNodes = map (uncurry toShift) shifts where
        toShift tok (state, _) = ShiftNode state tok
        
      reduceNodes = map (uncurry toReduce) reduces where
        toReduce tok rule = ReduceNode rule (Just tok)
        
      defaultNode = case defaultAction of
        ErrorShift state -> [ShiftNode state errorTok]
        Reduce rule -> [ReduceNode rule Nothing]
        _ -> []
      
    -- Successor relation
    succ :: RecognitionNode -> RecognitionNode -> Bool
    succ Init (Item _ True) = True
    succ (Item a _) (Item b _) = isInDirectCompletion g a b
    succ (Item item@(Lr0 rule _) _) (ReduceNode rule' _) = rule == rule' && dotIsAtRightEnd g item
    succ (Item item@(Lr0 rule _) _) (ShiftNode _ token) = hasTokenAfterDot g item && (tokenAfterDot g item) == token
    succ _ _ = False
    
    -- Create a graph from the nodes and their successor relation. Here, the nodes are encoded as integers.
    graph :: Int -> [RecognitionNode] -> (RecognitionNode -> RecognitionNode -> Bool) -> RecognitionGraph
    graph i nodes succ = (i, (fromJust $ elemIndex Init nodes, fromAdj adjacency), nodes) where
      adjacency = map (ap (,) neighbors) intNodes
      neighbors i = filter (intSucc i) intNodes
      intNodes = [0 .. (length nodes)-1]
      intSucc i j = succ (nodes !! i) (nodes !! j)

  -- Pretty-print a graph.
  showGraph :: Grammar -> RecognitionGraph -> String
  showGraph g (state, graph, nodes) = header ++ unlines (map showNode (zip [0..] nodes)) where
    header = "Graph of state " ++ show state ++ ":\n"
    showNode (i, node) = unlines (if (null successors) then [line1] else [line1, line2]) where
      successors = snd $ (toAdj (snd graph)) !! i
      line1 = "  " ++ show i ++ ": " ++ show' node
      line2 = "  -> " ++ unwords (map show successors)
      show' Init = "Init"
      show' (Item item isCore) = showItem g item ++ if isCore then " (core)" else ""
      show' (ShiftNode state token) = "Shift to state " ++ show state ++ " (on " ++ (token_names g) ! token ++ ")"
      show' r@(ReduceNode rule token) = "Reduce rule " ++ show rule ++ ": " ++ showProd g rule ++ " (on " ++ maybe "default" ((token_names g) !) token ++ ")"
