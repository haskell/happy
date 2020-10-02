module RADCodeGen_LALR where
  import Grammar
  import LALR
  import GenUtils
  import RADTools (showItem, showProd, lhs, prod)
  import RADStateGen
  import Control.Monad
  import Data.List
  import Data.Maybe
  import Data.Text (pack, unpack, replace)
  import GHC.Arr ((!), indices)
  

  data ParserType = Normal | Monad | MonadLexer deriving (Eq, Show)
  
  data GenOptions = GenOptions {
    ptype :: ParserType,
    
    wrapperType :: String, -- e.g. "Parser"
    errorTokenType :: String, -- e.g. "ErrorToken"
    
    showTypes :: Bool,
    comments :: Bool,
    
    rank2Types :: Bool, -- when True, all functions (including goto functions) which use or enclose a higher-rank-function are annotated with an explicit type.
    forallMatch :: String, -- the text which determines which types count as rank-2-types.
    
    header :: String,
    footer :: String
  } deriving Show
  
  mlex opts = ptype opts == MonadLexer
  
  dotleft (Lr0 rule dot) = Lr0 rule (dot-1)
  
  -------------------- GENCODE --------------------
  -- Generate the full code
  genCode :: GenOptions -> Grammar -> [LALRState] -> ActionTable -> GotoTable -> IO String
  genCode opts g states action goto = do
    return $ newlines 3 [languageFeatures, header', entryPoints', definitions', states', actions', footer'] where
      languageFeatures
        | rank2Types opts = newline $ map extension ["RankNTypes", "ScopedTypeVariables"]
        | otherwise = "" where
          extension str = "{-# LANGUAGE " ++ str ++ " #-}"
        
      header' = header opts
      entryPoints' = newlines 2 $ map (entryPoint opts g states) (starts g)
      definitions' = definitions opts g
      
      states' = newlines 2 $ map (genState opts g) states
      
      actions' = newlines 2 $ map (genAction opts g) [1..prods]
      prods = length (productions g) - 1
      footer' = footer opts
    

  -------------------- ENTRYPOINT --------------------  
  entryPoint :: GenOptions -> Grammar -> [LALRState] -> (String, Name, Name, Bool) -> String
  entryPoint opts g states (name, lhs, rhs, isPartial)
    | isPartial = newline [typedecl, definition]
    | otherwise = newline [typedecl, definition, parseEof]
    where
    typedecl
      | showTypes opts = fromMaybe "" $ fmap (((name ++ " :: ") ++) . correctP) (symboltype opts g rhs)
      | otherwise = ""
      
    correctP = if mlex opts then p else parser
      
    definition = case ptype opts of
      Normal -> common ++ paren (checkEof ++ "const") ++ maybeWhere
      Monad -> common ++ paren (checkEof ++ "const . " ++ returnP) ++ maybeWhere
      MonadLexer -> common ++ paren (checkEof ++ "const . " ++ returnP) ++ " []" ++ maybeWhere
    
    common = name ++ " = state" ++ show i ++ " "
    
    -- After finishing, eof must be parsed. This is because the accept-state may accept per default, which means eof still has to be verified. For partial parsers, this is not the case.
    maybeWhere = if isPartial then "" else " where"
    checkEof = if isPartial then "" else "parseEof . "
    parseEof
      | mlex opts = newline [lex1, lex2, lex3]
      | otherwise = newline [normal1, normal2]
      where
        lex1 = "  parseEof k = lexerWrapper $ \\t -> case t of"
        lex2 = "    " ++ paren eof ++ " -> k"
        lex3 = "    _ -> happyErrorWrapper t"
        Just (_, eof) = lexer g
        normal1 = "  parseEof k [] = k []"
        normal2 = "  parseEof k ts = " ++ happyError ++ " ts"
        happyError = fromMaybe "happyError" (error_handler g)
    
    -- Rule LHS -> RHS
    prod = fromJust $ find matches [0 .. length (productions g) - 1] where
      matches i = matches' (lookupProdNo g i)
      matches' (Production lhs' rhs' _ _) = lhs' == lhs && rhs' == [rhs]
    
    -- State with item LHS -> . RHS
    state = fromJust $ find (matches . coreItems) states where -- state with item LHS -> . RHS
      matches items = elem (Lr0 prod 0) items
    i = fromJust $ elemIndex state states
      
    
    p a = p' ++ " " ++ a
    parser a = wrapperType opts ++ " " ++ a
    (_, _, p', _, returnP) = monad g


  -------------------- DEFINITIONS --------------------
  -- Generate definitions such as wrappers, the parser type or more required functions and types
  definitions :: GenOptions -> Grammar -> String
  definitions opts g = case ptype opts of
    Normal -> newlines 2 [parserDecl, errorToken]
    Monad -> newlines 2 [parserDecl, errorToken, wrapThen]
    MonadLexer -> newlines 2 [parserDecl, errorToken, wrapThen, repeatTok, wrapLexer, wrapError]
    where
      
      -- type Parser r = [Token] -> P r
      parserDecl
        | ptype opts == Normal = "type " ++ parser "r" ++ " = [" ++ tokenT ++ "] -> r"
        | otherwise = "type " ++ parser "r" ++ " = [" ++ tokenT ++ "] -> " ++ p "r"
      
      -- data ErrorToken = ErrorToken
      errorToken = "data " ++ errorTokenT ++ " = " ++ errorTokenT
      
      -- thenWrapP :: P a -> (a -> Parser b) -> Parser b
      -- thenWrapP a f ts = (thenP) a (flip f ts)
      wrapThen = newline [typedecl, definition] where
        name = "thenWrapP"
        typedecl = name ++ " :: " ++ p "a" ++ " -> (a -> " ++ parser "b" ++ ") -> " ++ parser "b"
        definition = name ++ " a f ts = " ++ paren thenP ++ " a (flip f ts)"

      -- repeatTok :: Token -> Parser a -> Parser a
      -- repeatTok tok p = \cur -> p (tok:cur)
      repeatTok = newline [typedecl, definition] where
        name = "repeatTok"
        typedecl = name ++ " :: " ++ tokenT ++ " -> " ++ parser "a" ++ " -> " ++ parser "a"
        definition = name ++ " tok p = \\cur -> p (tok:cur)"
      
      -- lexerWrapper :: (Token -> Parser a) -> Parser a
      -- lexerWrapper cont [] = lexer (\tok -> cont tok [])
      -- lexerWrapper cont (tok:toks) = cont tok toks
      wrapLexer = newline [typedecl, line1, line2] where
        name = "lexerWrapper"
        typedecl = name ++ " :: " ++ paren (tokenT ++ " -> " ++ parser "a") ++ " -> " ++ parser "a"
        line1 = name ++ " cont [] = " ++ lexer' ++ " (\\t -> cont t [])"
        line2 = name ++ " cont (t:ts) = cont t ts"

      -- happyErrorWrapper :: Token -> Parser a
      -- happyErrorWrapper t _ = happyError t
      wrapError = newline [typedecl, definition] where
        name = "happyErrorWrapper"
        typedecl = name ++ " :: " ++ tokenT ++ " -> " ++ parser "a"
        definition = name ++ " = const . " ++ happyError
      
      p a = p' ++ " " ++ a
      parser a = wrapperType opts ++ " " ++ a
      (_, _, p', thenP, _) = monad g
      tokenT = token_type g
      errorTokenT = errorTokenType opts
      (Just (lexer', _)) = lexer g
      happyError = fromMaybe "happyError" (error_handler g)
    

  -------------------- GENSTATE -------------------
  -- Generate the code for a single state.
  genState :: GenOptions -> Grammar -> LALRState -> String
  genState opts g state
    | isTrivial = newline [comment, trivialTypedecl, trivialHeader]
    | otherwise = newline [comment, typedecl, header, shifts', reduces', defaultAction', gotos'] where
    
    isTrivial = (length (coreItems state) == 1) && (null $ shifts state) && (null $ gotos state) && (null $ reduces state) && isReduce (defaultAction state) where
      isReduce (Reduce _) = True
      isReduce _ = False
      
    hasRank2Goto = any ((hasRank2Type opts g) . fst) (gotos state)
    hasRank2TypeSignature = any (hasRank2Item) (coreItems state)
    hasRank2Item item = any (hasRank2Type opts g) (rhsAfterDot g item)
    
    trivialTypedecl
      | rank2Types opts && hasRank2Goto = fromMaybe "" (stateTypeSignature opts g True state)
      | otherwise = ""
      
    trivialHeader = "state" ++ show (index state) ++ " = id"
    
    comment
      | comments opts = newlineMap "-- " (showItem g) (coreItems state)
      | otherwise = ""
    
    typedecl
      | rank2Types opts && hasRank2Goto = fromMaybe "" (stateTypeSignature opts g True state)
      | rank2Types opts && hasRank2TypeSignature = fromMaybe "" (stateTypeSignature opts g False state)
      | showTypes opts = fromMaybe "" (stateTypeSignature opts g False state)
      | otherwise = ""
      
    shifts' = newlineMap "  " shift (shifts state)
    reduces' = newlineMap "  " reduce (reduces state)
    gotos' = where' ++ intercalate "\n" (map ("    " ++) lines) where
      lines = join (map goto (gotos state))
      where' = if null (gotos state) then "" else "  where\n"
    
    header
      | mlex opts = common ++ " = lexerWrapper $ \\t -> case t of"
      | otherwise = common ++ " ts = case ts of" where
        common = "state" ++ show (index state) ++ " " ++ headerKs
        headerKs = unwords $ map k (coreItems state)
        
    shift (token, (state, i))
      | mlex opts = paren tok ++ " -> state" ++ show state ++ " " ++ kcontent
      | otherwise = "t@" ++ paren tok ++ ":tr -> state" ++ show state ++ " " ++ kcontent ++ " tr" where
        i' = map dotleft i
        tok = replaceDollar rawToken (if wantsProjection then "v" else "_")
        replaceDollar a char = maybe a ($ char) (mapDollarDollar a)
        kcontent = unwords (map (paren . (++ x) . k) i') where
          x = if wantsProjection then " v" else " t"
        rawToken = fromJust $ lookup token (token_specs g)
        wantsProjection = "$$" == (rawToken \\ replaceDollar rawToken "") -- i.e. Tokens of form "TokenInt $$"
      
    reduce (token, rule)
      | mlex opts = paren tokMaybeEof ++ " -> repeatTok t $ " ++ k'
      | otherwise = if token == eof_term g then eofCase else normalCase
      where
        normalCase = "t@" ++ paren tok ++ ":tr -> " ++ k' ++ " ts"
        eofCase = "[] -> " ++ k' ++ " ts"
        
        tokMaybeEof = if token == eof_term g then eof else tok
        tok = removeDollar $ fromJust (lookup token (token_specs g))
        Just (_, eof) = lexer g
        
        removeDollar a = maybe a ($ "_") (mapDollarDollar a)
        k' = let dot = length (rhs (lookupProdNo g rule)) in k (Lr0 rule dot)
      
    goto (nt, (state, i))
      | hasRank2Type opts g nt = catMaybes [gototype, goto]
      | otherwise = catMaybes [goto]
      where
        i' = map dotleft i
        gototype = case symboltype opts g nt of
          Just t -> Just $ "g" ++ show nt ++ " :: " ++ t ++ " -> " ++ paren outtype
          Nothing -> Nothing
        goto = Just $ "g" ++ show nt ++ " x = state" ++ show state ++ " " ++ unwords (map (paren . (++ " x") . k) i')
        outtype = wrapperType opts ++ " r"
      
    defaultAction' = "  " ++ case defaultAction state of
      ErrorShift state -> defaultErrorShift state
      Reduce rule -> defaultReduce rule
      Error -> defaultError
      
    defaultErrorShift toState
      | mlex opts = "_ -> repeatTok t $ state" ++ show toState ++ " " ++ paren (k item ++ " ErrorToken")
      | otherwise = "_ -> state" ++ show toState ++ " " ++ paren (k item ++ " ErrorToken") ++ " ts" where
        item = head $ hdiv (completionItems state) errorTok g
    
    defaultReduce rule
      | mlex opts = "_ -> repeatTok t $ " ++ k'
      | otherwise = "_ -> " ++ k' ++ " ts" where
        k' = let dot = length (rhs (lookupProdNo g rule)) in k (Lr0 rule dot)
      
    defaultError
      | mlex opts = "_ -> happyErrorWrapper t"
      | otherwise = "_ -> " ++ happyError ++ " ts" where
      happyError = fromMaybe "happyError" (error_handler g)
    
    k item@(Lr0 rule dot) = maybe noCore core $ elemIndex item (coreItems state) where
      core idx
        | (length (coreItems state) == 1) = "k"
        | otherwise = "k" ++ (show (idx + 1))
      noCore = "action" ++ show rule ++ " g" ++ show (lhs g item)

   -- Create the type signature for a state.
  stateTypeSignature :: GenOptions -> Grammar -> Bool -> LALRState -> Maybe String
  stateTypeSignature opts g forall_r state = do
    let start = "state" ++ show (index state) ++ " :: " ++ forall
    components <- mapM component (coreItems state)
    return $ start ++ intercalate " -> " (map paren (components ++ [outtype]))
    where
      component item = fmap (intercalate " -> " . (++ [outtype])) (mapM (symboltype opts g) (rhsAfterDot g item))
      outtype = wrapperType opts ++ " r"
      forall = if forall_r then "forall r. " else ""


  -------------------- GENACTION --------------------
  -- Create the code for a semantic action, i.e. a reduce action.
  genAction :: GenOptions -> Grammar -> Int -> String
  genAction opts g i = newline [comment, typedecl, code] where
    prod@(Production lhs' rhs' _ _) = lookupProdNo g i
    
    comment
      | comments opts = "-- " ++ showProd g i
      | otherwise = ""
      
    typedecl
      | showTypes opts || rank2Types opts = typedecl' -- some actions (not further specified) need to be explicitly typed in order for rank-n-types to work
      | otherwise = ""
      where
        typedecl' = fromMaybe "" $ fmap (("action" ++ show i ++ " :: ") ++) (actionTypedecl opts g i)
    
    code = header ++ (if isMonadic then monadicCode else normalCode)
    (customCode, isMonadic) = customProdCode prod
    header = "action" ++ show i ++ " g " ++ unwords (map v [1..length rhs']) ++ " = "
    normalCode = "g " ++ paren customCode
    monadicCode = paren customCode ++ " `thenWrapP` g"
    v n = "v" ++ show n
    
  -- Generate the type signature of a semantic action function.
  actionTypedecl :: GenOptions -> Grammar -> Int -> Maybe String
  actionTypedecl opts g i = do
    lhstype <- symboltype opts g lhs'
    let lhs = paren $ intercalate " -> " $ [lhstype, outtype]
    rhstypes <- mapM (symboltype opts g) rhs'
    let rhs = intercalate " -> " $ rhstypes ++ [outtype]
    return (lhs ++ " -> " ++ rhs)
    where
      Production lhs' rhs' _ _ = lookupProdNo g i
      outtype = wrapperType opts ++ " r"
      
  -- Read and translate the raw action code supplied by the user. Also return whether the action is monadic or not.
  customProdCode :: Production -> (String, Bool)
  customProdCode (Production _ _ (code, _) _) = case code of
    '%':'%':_ -> error "'{%%' actions not supported"
    '%':'^':_ -> error "'{%^' actions not supported"
    '%':rest -> (adapt rest, True)
    _ -> (adapt code, False)
    where
      adapt code
        | code == "no code" = v 1
        | otherwise = replaceHappyVars code
      v n = "v" ++ show n
      replaceHappyVars = unpack . replace (pack "happy_var_") (pack "v") . pack
  

  -------------------- TOOLS --------------------
  
  -- Insert newlines between the strings; ignore empty strings
  newlines :: Int -> [String] -> String
  newlines n = intercalate (replicate n '\n') . filter (not . null)
  
  newline = newlines 1
  
  newlineMap prefix f x = newlines 1 $ map ((prefix ++) . f) x
  
  paren a = "(" ++ a ++ ")"
  
  rhsAfterDot g item@(Lr0 rule dot) = drop dot $ rhs (prod g item)
  rhs (Production _ rhs _ _) = rhs
  
  hasRank2Type opts g nt = rank2Types opts && case symboltype opts g nt of
    Just t -> isInfixOf (forallMatch opts) t
    Nothing -> False
      
  symboltype opts g symbol
    | symbol == errorTok = Just (process $ errorTokenType opts)
    | elem symbol (non_terminals g) = fmap process $ join (maybelookup (types g) symbol)
    | wantsProjection = Nothing -- we don't know the type of the projection
    | otherwise = Just (process $ token_type g)
    where
    process = remNewlines . paren where
      remNewlines = map replace
      replace '\n' = ' '
      replace x = x
    maybelookup arr i = if elem i (indices arr) then Just (arr ! i) else Nothing
    wantsProjection = "$$" == (rawToken \\ replaceDollar rawToken "") -- i.e. Tokens of form "TokenInt $$"
    rawToken = fromJust $ lookup symbol (token_specs g)
    replaceDollar a char = maybe a ($ char) (mapDollarDollar a)