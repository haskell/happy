/-----------------------------------------------------------------------------
The Grammar data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Mangler converts AbsSyn to Grammar

> module Happy.Frontend.Mangler (mangler) where

> import Happy.Core.Grammar
> import Happy.Core.GenUtils
> import Happy.Frontend.AbsSyn
#ifdef HAPPY_BOOTSTRAP
> import Happy.Frontend.ParseMonad.Class
> import Happy.Frontend.AttrGrammar
#endif

This is only supported in the bootstrapped version
#ifdef HAPPY_BOOTSTRAP
> import Happy.Frontend.AttrGrammarParser
> import Data.List     ( findIndices, groupBy, intersperse, nub, sortBy )
> import Control.Monad ( when )
#endif

> import Happy.Frontend.ParamRules

> import Data.Array ( Array, (!), accumArray, array, listArray )
> import Data.Char  ( isAlphaNum, isDigit, isLower )
> import Data.List  ( zip4 )
> import Data.Maybe ( fromMaybe )

> import Control.Monad.Writer ( Writer, MonadWriter(..), mapWriter, runWriter )

-----------------------------------------------------------------------------
-- The Mangler

This bit is a real mess, mainly because of the error message support.

> type ErrMsg = String
> type M a = Writer [ErrMsg] a

> addErr :: ErrMsg -> M ()
> addErr e = tell [e]

> mangler :: FilePath -> AbsSyn -> Either [ErrMsg] Grammar
> mangler file abssyn
>   | null errs = Right g
>   | otherwise = Left errs
>   where (g, errs) = runWriter (manglerM file abssyn)

> manglerM :: FilePath -> AbsSyn -> M Grammar
> manglerM file (AbsSyn _hd dirs rules' _tl) =
>   -- add filename to all error messages
>   mapWriter (\(a,e) -> (a, map (\s -> file ++ ": " ++ s) e)) $ do

>   rules <- case expand_rules rules' of
>              Left err -> addErr err >> return []
>              Right as -> return as
>   nonterm_strs <- checkRules [n | Rule1 n _ _ <- rules] "" []

>   let

>       terminal_strs  = concat (map getTerm dirs) ++ [eofName]

>       n_starts   = length starts'
>       n_nts      = length nonterm_strs
>       n_ts       = length terminal_strs
>       first_nt   = firstStartTok + n_starts
>       first_t    = first_nt + n_nts
>       last_start = first_nt - 1
>       last_nt    = first_t  - 1
>       last_t     = first_t + n_ts - 1

>       start_names    = [ firstStartTok .. last_start ]
>       nonterm_names  = [ first_nt .. last_nt ]
>       terminal_names = [ first_t .. last_t ]

>       starts'     = case getParserNames dirs of
>                       [] -> [TokenName "happyParse" Nothing False]
>                       ns -> ns
>
>       start_strs  = [ startName++'_':p  | (TokenName p _ _) <- starts' ]

Build up a mapping from name values to strings.

>       name_env = (errorTok, errorName) :
>                  (dummyTok, dummyName) :
>                  zip start_names    start_strs ++
>                  zip nonterm_names  nonterm_strs ++
>                  zip terminal_names terminal_strs

>       lookupName :: String -> [Name]
>       lookupName n = [ t | (t,r) <- name_env, r == n ]

>       mapToName str' =
>             case lookupName str' of
>                [a]   -> return a
>                []    -> do addErr ("unknown identifier '" ++ str' ++ "'")
>                            return errorTok
>                (a:_) -> do addErr ("multiple use of '" ++ str' ++ "'")
>                            return a

Start symbols...

>               -- default start token is the first non-terminal in the grammar
>       lookupStart (TokenName _ Nothing  _) = return first_nt
>       lookupStart (TokenName _ (Just n) _) = mapToName n
>       lookupStart _ = error "lookupStart: Not a TokenName"
>   -- in

>   start_toks <- mapM lookupStart starts'

>   let
>       parser_names   = [ s | TokenName s _ _ <- starts' ]
>       start_partials = [ b | TokenName _ _ b <- starts' ]
>       start_prods = zipWith (\nm tok -> Production nm [tok] ("no code",[]) No)
>                        start_names start_toks

Deal with priorities...

>       priodir = zip [1..] (getPrios dirs)
>
>       mkPrio :: Int -> Directive a -> Priority
>       mkPrio i (TokenNonassoc _) = Prio None i
>       mkPrio i (TokenRight _) = Prio RightAssoc i
>       mkPrio i (TokenLeft _) = Prio LeftAssoc i
>       mkPrio _ _ = error "Panic: impossible case in mkPrio"

>       prios = [ (name,mkPrio i dir)
>               | (i,dir) <- priodir
>               , nm <- getPrioNames dir
>               , name <- lookupName nm
>               ]

>       prioByString = [ (name, mkPrio i dir)
>                      | (i,dir) <- priodir
>                      , name <- getPrioNames dir
>                      ]

Translate the rules from string to name-based.

>       convNT (Rule1 nt prods ty)
>         = do nt' <- mapToName nt
>              return (nt', prods, ty)
>
>       attrs = getAttributes dirs
>       attrType = fromMaybe "HappyAttrs" (getAttributetype dirs)
>
>       transRule (nt, prods, _ty)
>         = mapM (finishRule nt) prods
>
>       finishRule :: Name -> Prod1 -> Writer [ErrMsg] Production
>       finishRule nt (Prod1 lhs code line prec)
>         = mapWriter (\(a,e) -> (a, map (addLine line) e)) $ do
>           lhs' <- mapM mapToName lhs
>           code' <- checkCode (length lhs) lhs' nonterm_names code attrs
>           case mkPrec lhs' prec of
>               Left s  -> do addErr ("Undeclared precedence token: " ++ s)
>                             return (Production nt lhs' code' No)
>               Right p -> return (Production nt lhs' code' p)
>
>       mkPrec :: [Name] -> Prec -> Either String Priority
>       mkPrec lhs PrecNone =
>         case filter (flip elem terminal_names) lhs of
>                            [] -> Right No
>                            xs -> case lookup (last xs) prios of
>                                    Nothing -> Right No
>                                    Just p  -> Right p
>       mkPrec _ (PrecId s) =
>         case lookup s prioByString of
>                           Nothing -> Left s
>                           Just p -> Right p
>
>       mkPrec _ PrecShift = Right PrioLowest
>
>   -- in

>   rules1 <- mapM convNT rules
>   rules2 <- mapM transRule rules1

>   let
>       type_env = [(nt, t) | Rule1 nt _ (Just (t,[])) <- rules] ++
>                  [(nt, getTokenType dirs) | nt <- terminal_strs] -- XXX: Doesn't handle $$ type!
>
>       fixType (ty,s) = go "" ty
>         where go acc [] = return (reverse acc)
>               go acc (c:r) | isLower c = -- look for a run of alphanumerics starting with a lower case letter
>                                let (cs,r1) = span isAlphaNum r
>                                    go1 x = go (reverse x ++ acc) r1
>                                in case lookup (c:cs) s of
>                                        Nothing -> go1 (c:cs) -- no binding found
>                                        Just a -> case lookup a type_env of
>                                          Nothing -> do
>                                            addErr ("Parameterized rule argument '" ++ a ++ "' does not have type")
>                                            go1 (c:cs)
>                                          Just t -> go1 $ "(" ++ t ++ ")"
>                            | otherwise = go (c:acc) r
>
>       convType (nm, t)
>         = do t' <- fixType t
>              return (nm, t')
>
>   -- in
>   tys <- mapM convType [ (nm, t) | (nm, _, Just t) <- rules1 ]
>

>   let
>       type_array :: Array Int (Maybe String)
>       type_array = accumArray (\_ x -> x) Nothing (first_nt, last_nt)
>                    [ (nm, Just t) | (nm, t) <- tys ]

>       env_array :: Array Int String
>       env_array = array (errorTok, last_t) name_env
>   -- in

Get the token specs in terms of Names.

>   let
>       fixTokenSpec (a,b) = do n <- mapToName a; return (n,b)
>   -- in
>   tokspec <- mapM fixTokenSpec (getTokenSpec dirs)

>   let
>      ass = combinePairs [ (a,no)
>                         | (Production a _ _ _,no) <- zip productions' [0..] ]
>      arr = array (firstStartTok, length ass - 1 + firstStartTok) ass

>      lookup_prods :: Name -> [Int]
>      lookup_prods x | x >= firstStartTok && x < first_t = arr ! x
>      lookup_prods _ = error "lookup_prods"
>
>      productions' = start_prods ++ concat rules2
>      prod_array  = listArray (0,length productions' - 1) productions'
>   -- in

>   return  (Grammar {
>               productions       = productions',
>               lookupProdNo      = (prod_array !),
>               lookupProdsOfName = lookup_prods,
>               token_specs       = tokspec,
>               terminals         = errorTok : terminal_names,
>               non_terminals     = start_names ++ nonterm_names,
>                                       -- INCLUDES the %start tokens
>               starts            = zip4 parser_names start_names start_toks
>                                       start_partials,
>               types             = type_array,
>               token_names       = env_array,
>               first_nonterm     = first_nt,
>               first_term        = first_t,
>               eof_term          = last terminal_names,
>               priorities        = prios,
>               imported_identity                 = getImportedIdentity dirs,
>               monad             = getMonad dirs,
>               lexer             = getLexer dirs,
>               error_handler     = getError dirs,
>               error_sig         = getErrorHandlerType dirs,
>               token_type        = getTokenType dirs,
>               expect            = getExpect dirs,
>               attributes        = attrs,
>               attributetype     = attrType,
>               hd                = _hd,
>               tl                = _tl
>       })

For combining actions with possible error messages.

> addLine :: Int -> String -> String
> addLine l s = show l ++ ": " ++ s

> getTerm :: Directive a -> [a]
> getTerm (TokenSpec stuff) = map fst stuff
> getTerm _                 = []

So is this.

> checkRules :: [String] -> String -> [String] -> Writer [ErrMsg] [String]
> checkRules (name:rest) above nonterms
>       | name == above = checkRules rest name nonterms
>       | name `elem` nonterms
>               = do addErr ("Multiple rules for '" ++ name ++ "'")
>                    checkRules rest name nonterms
>       | otherwise = checkRules rest name (name : nonterms)

> checkRules [] _ nonterms = return (reverse nonterms)

-----------------------------------------------------------------------------
-- If any attribute directives were used, we are in an attribute grammar, so
-- go do special processing.  If not, pass on to the regular processing routine

> checkCode :: Int -> [Name] -> [Name] -> String -> [(String,String)] -> M (String,[Int])
> checkCode arity _   _             code []    = doCheckCode arity code

#ifdef HAPPY_BOOTSTRAP
> checkCode arity lhs nonterm_names code attrs = rewriteAttributeGrammar arity lhs nonterm_names code attrs
#else
> checkCode arity _   _             code (_:_) = do
>   addErr "Attribute grammars are not supported in non-bootstrapped build"
>   doCheckCode arity code
#endif

------------------------------------------------------------------------------
-- Special processing for attribute grammars.  We re-parse the body of the code
-- block and output the nasty-looking record manipulation and let binding goop
--

#ifdef HAPPY_BOOTSTRAP

> rewriteAttributeGrammar :: Int -> [Name] -> [Name] -> String -> [(String,String)] -> M (String,[Int])
> rewriteAttributeGrammar arity lhs nonterm_names code attrs =

   first we need to parse the body of the code block

>     case runFromStartP agParser code 0 of
>        Left msg  -> do addErr ("error in attribute grammar rules: "++msg)
>                        return ("",[])
>        Right rules  ->

   now we break the rules into three lists, one for synthesized attributes,
   one for inherited attributes, and one for conditionals

>            let (selfRules,subRules,conditions) = partitionRules [] [] [] rules
>                attrNames = map fst attrs
>                defaultAttr = head attrNames

   now check that $i references are in range

>            in do let prods = mentionedProductions rules
>                  mapM_ checkArity prods

   and output the rules

>                  rulesStr <- formatRules arity attrNames defaultAttr
>                               allSubProductions selfRules
>                               subRules conditions

   return the munged code body and all sub-productions mentioned

>                  return (rulesStr,nub (allSubProductions++prods))


>    where partitionRules a b c [] = (a,b,c)
>          partitionRules a b c (RightmostAssign attr toks : xs) = partitionRules a (SubAssign (arity,attr) toks : b) c xs
>          partitionRules a b c (x@(SelfAssign _ _ )  : xs) = partitionRules (x:a) b c xs
>          partitionRules a b c (x@(SubAssign _ _)    : xs) = partitionRules a (x:b) c xs
>          partitionRules a b c (x@(Conditional _)    : xs) = partitionRules a b (x:c) xs

>          allSubProductions             = map (+1) (findIndices (`elem` nonterm_names) lhs)

>          mentionedProductions rules    = [ i | (AgTok_SubRef (i,_)) <- concat (map getTokens rules) ]

>          getTokens (SelfAssign _ toks)      = toks
>          getTokens (SubAssign _ toks)       = toks
>          getTokens (Conditional toks)       = toks
>          getTokens (RightmostAssign _ toks) = toks
>
>          checkArity x = when (x > arity) $ addErr (show x++" out of range")


------------------------------------------------------------------------------------
-- Actually emit the code for the record bindings and conditionals
--

> formatRules :: Int -> [String] -> String -> [Name]
>             -> [AgRule] -> [AgRule] -> [AgRule]
>             -> M String

> formatRules arity _attrNames defaultAttr prods selfRules subRules conditions = return $
>     concat [ "\\happyInhAttrs -> let { "
>            , "happySelfAttrs = happyInhAttrs",formattedSelfRules
>            , subProductionRules
>            , "; happyConditions = ", formattedConditions
>            , " } in (happyConditions,happySelfAttrs)"
>            ]
>
>  where formattedSelfRules = case selfRules of [] -> []; _ -> "{ "++formattedSelfRules'++" }"
>        formattedSelfRules' = concat $ intersperse ", " $ map formatSelfRule selfRules
>        formatSelfRule (SelfAssign [] toks)   = defaultAttr++" = "++(formatTokens toks)
>        formatSelfRule (SelfAssign attr toks) = attr++" = "++(formatTokens toks)
>        formatSelfRule _ = error "formatSelfRule: Not a self rule"

>        subRulesMap :: [(Int,[(String,[AgToken])])]
>        subRulesMap = map     (\l   -> foldr (\ (_,x) (i,xs) -> (i,x:xs))
>                                             (fst $ head l,[snd $ head l])
>                                             (tail l) ) .
>                      groupBy (\x y -> (fst x) == (fst y)) .
>                      sortBy  (\x y -> compare (fst x) (fst y)) .
>                      map     (\(SubAssign (i,ident) toks) -> (i,(ident,toks))) $ subRules

>        subProductionRules = concat $ map formatSubRules prods

>        formatSubRules i =
>           let attrs = fromMaybe [] . lookup i $ subRulesMap
>               attrUpdates' = concat $ intersperse ", " $ map (formatSubRule i) attrs
>               attrUpdates  = case attrUpdates' of [] -> []; x -> "{ "++x++" }"
>           in concat ["; (happyConditions_",show i,",happySubAttrs_",show i,") = ",mkHappyVar i
>                     ," happyEmptyAttrs"
>                     , attrUpdates
>                     ]
>
>        formattedConditions = concat $ intersperse " Prelude.++ " $ localConditions : (map (\i -> "happyConditions_"++(show i)) prods)
>        localConditions = "["++(concat $ intersperse ", " $ map formatCondition conditions)++"]"
>        formatCondition (Conditional toks) = formatTokens toks
>        formatCondition _ = error "formatCondition: Not a condition"

>        formatSubRule _ ([],toks)   = defaultAttr++" = "++(formatTokens toks)
>        formatSubRule _ (attr,toks) = attr++" = "++(formatTokens toks)

>        formatTokens tokens = concat (map formatToken tokens)

>        formatToken AgTok_LBrace           =  "{ "
>        formatToken AgTok_RBrace           = "} "
>        formatToken AgTok_Where            = "where "
>        formatToken AgTok_Semicolon        = "; "
>        formatToken AgTok_Eq               = "="
>        formatToken (AgTok_SelfRef [])     = "("++defaultAttr++" happySelfAttrs) "
>        formatToken (AgTok_SelfRef x)      = "("++x++" happySelfAttrs) "
>        formatToken (AgTok_RightmostRef x) = formatToken (AgTok_SubRef (arity,x))
>        formatToken (AgTok_SubRef (i,[]))
>            | i `elem` prods = "("++defaultAttr++" happySubAttrs_"++(show i)++") "
>            | otherwise      = mkHappyVar i ++ " "
>        formatToken (AgTok_SubRef (i,x))
>            | i `elem` prods = "("++x++" happySubAttrs_"++(show i)++") "
>            | otherwise      = error ("lhs "++(show i)++" is not a non-terminal")
>        formatToken (AgTok_Unknown x)     = x++" "
>        formatToken AgTok_EOF = error "formatToken AgTok_EOF"

#endif


-----------------------------------------------------------------------------
-- Check for every $i that i is <= the arity of the rule.

-- At the same time, we collect a list of the variables actually used in this
-- code, which is used by the backend.

> doCheckCode :: Int -> String -> M (String, [Int])
> doCheckCode arity code0 = go code0 "" []
>   where go code acc used =
>           case code of
>               [] -> return (reverse acc, used)
>
>               '"'  :r    -> case reads code :: [(String,String)] of
>                                []       -> go r ('"':acc) used
>                                (s,r'):_ -> go r' (reverse (show s) ++ acc) used
>               a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc) used
>               '\'' :r    -> case reads code :: [(Char,String)] of
>                                []       -> go r  ('\'':acc) used
>                                (c,r'):_ -> go r' (reverse (show c) ++ acc) used
>               '\\':'$':r -> go r ('$':acc) used
>
>               '$':'>':r -- the "rightmost token"
>                       | arity == 0 -> do addErr "$> in empty rule"
>                                          go r acc used
>                       | otherwise  -> go r (reverse (mkHappyVar arity) ++ acc)
>                                        (arity : used)
>
>               '$':r@(i:_) | isDigit i ->
>                       case reads r :: [(Int,String)] of
>                         (j,r'):_ ->
>                            if j > arity
>                                 then do addErr ('$': show j ++ " out of range")
>                                         go r' acc used
>                                 else go r' (reverse (mkHappyVar j) ++ acc)
>                                        (j : used)
>                         [] -> error "doCheckCode []"
>               c:r  -> go r (c:acc) used

> mkHappyVar :: Int -> String
> mkHappyVar n  = "happy_var_" ++ show n