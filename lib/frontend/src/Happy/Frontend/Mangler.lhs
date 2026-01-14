/-----------------------------------------------------------------------------
The Grammar data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Mangler converts AbsSyn to Grammar

> {-# LANGUAGE ScopedTypeVariables #-}

> module Happy.Frontend.Mangler (mangler) where

> import Happy.Grammar
> import Happy.Frontend.AbsSyn
> import Happy.Frontend.Mangler.Monad
> import Happy.Frontend.AttrGrammar.Mangler

> import Happy.Frontend.ParamRules

> import Data.Array ( Array, (!), accumArray, array, listArray )
> import Data.Char  ( isAlphaNum, isDigit, isLower )
> import Data.List  ( zip4, sortBy )
> import Data.Ord

> import Control.Monad.Writer ( Writer, mapWriter, runWriter )

-----------------------------------------------------------------------------
-- The Mangler

This bit is a real mess, mainly because of the error message support.

> mangler :: FilePath -> AbsSyn String -> Either [ErrMsg] (Grammar String, Maybe AttributeGrammarExtras, Directives)
> mangler file abssyn@(AbsSyn dirs _)
>   | null errs = Right (gd, mAg, ps)
>   | otherwise = Left errs
>   where mAg = getAttributeGrammarExtras dirs
>         ((gd, ps), errs) = runWriter (manglerM "no code" checkCode file abssyn)

If any attribute directives were used, we are in an attribute grammar, so
go do special processing.  If not, pass on to the regular processing routine

>         checkCode :: CodeChecker String
>         checkCode = case mAg of
>             Nothing -> \lhs _             code ->
>                 doCheckCode (length lhs) code
>             Just a  -> \lhs nonterm_names code ->
>                 rewriteAttributeGrammar lhs nonterm_names code a

> -- | Function to check elimination rules
> type CodeChecker e = [Name] -> [Name] -> e -> M (e, [Int])

> manglerM
>   :: forall e
>   .  e
>   -- ^ Empty elimination rule, used for starting productions. Will never be run.
>   -> CodeChecker e
>   -> FilePath
>   -> AbsSyn e
>   -> M (Grammar e, Directives)
> manglerM noCode checkCode file (AbsSyn dirs rules') =
>   -- add filename to all error messages
>   mapWriter (\(a,e) -> (a, map (\s -> file ++ ": " ++ s) e)) $ do

>   rules <- case expand_rules rules' of
>              Left err -> addErr err >> return []
>              Right as -> return as
>   nonterm_strs <- checkRules [n | Rule1 n _ _ <- rules] "" []

>   let

>       terminal_strs  = concat (map getTerm dirs) ++ [eofName]

>       first_nt, first_t, last_start, last_nt, last_t :: Name

>       first_nt   = MkName $ getName firstStartTok + length starts'
>       first_t    = MkName $ getName first_nt + length nonterm_strs
>       last_start = MkName $ getName first_nt - 1
>       last_nt    = MkName $ getName first_t  - 1
>       last_t     = MkName $ getName first_t + length terminal_strs - 1

>       start_names    = [ firstStartTok .. last_start ]
>       nonterm_names  = [ first_nt .. last_nt ]
>       terminal_names = [ first_t .. last_t ]

>       starts'     = case getParserNames dirs of
>                       [] -> [TokenName "happyParse" Nothing False]
>                       ns -> ns
>       error_resumptive | ResumptiveErrorHandler{} <- getError dirs = True
>                        | otherwise                                 = False

>       start_strs  = [ startName++'_':p  | (TokenName p _ _) <- starts' ]

Build up a mapping from name values to strings.

>       name_env = (errorTok, errorName) :
>                  (catchTok, catchName) :
>                  (dummyTok, dummyName) :
>                  zip start_names    start_strs ++
>                  zip nonterm_names  nonterm_strs ++
>                  zip terminal_names terminal_strs

>       lookupName :: String -> [Name]
>       lookupName n = [ t | (t,r) <- name_env, r == n
>                          , t /= catchTok || error_resumptive ]
>                            -- hide catchName unless %errorresumptive is active
>                            -- issue93.y uses catch as a nonterminal, we should not steal it

>       mapToName str' =
>             case lookupName str' of
>                [a]   -> return a
>                []    -> do addErr ("unknown identifier '" ++ str' ++ "'")
>                            return errorTok -- SG: What a confusing use of errorTok.. Use dummyTok?
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
>       start_prods = zipWith (\nm tok -> Production nm [tok] (noCode,[]) No)
>                        start_names start_toks

Deal with priorities...

>       priodir = zip [1..] (getPrios dirs)

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

>       transRule (nt, prods, _ty)
>         = mapM (finishRule nt) prods

>       finishRule :: Name -> Prod1 e -> Writer [ErrMsg] (Production e)
>       finishRule nt (Prod1 lhs code line prec)
>         = mapWriter (\(a,e) -> (a, map (addLine line) e)) $ do
>           lhs' <- mapM mapToName lhs
>           code' <- checkCode lhs' nonterm_names code
>           case mkPrec lhs' prec of
>               Left s  -> do addErr ("Undeclared precedence token: " ++ s)
>                             return (Production nt lhs' code' No)
>               Right p -> return (Production nt lhs' code' p)

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

>       mkPrec _ PrecShift = Right PrioLowest

>   -- in

>   rules1 <- mapM convNT rules
>   rules2 <- mapM transRule rules1

>   let
>       type_env = [(nt, t) | Rule1 nt _ (Just (t,[])) <- rules] ++
>                  [(nt, getTokenType dirs) | nt <- terminal_strs] -- XXX: Doesn't handle $$ type!

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

>       convType (nm, t)
>         = do t' <- fixType t
>              return (nm, t')

>   -- in
>   tys <- mapM convType [ (nm, t) | (nm, _, Just t) <- rules1 ]


>   let
>       type_array :: Array Name (Maybe String)
>       type_array = accumArray (\_ x -> x) Nothing (first_nt, last_nt)
>                    [ (nm, Just t) | (nm, t) <- tys ]

>       env_array :: Array Name String
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
>      arr = array (firstStartTok, MkName $ length ass - 1 + getName firstStartTok) ass

>      lookup_prods :: Name -> [Int]
>      lookup_prods x | x >= firstStartTok && x < first_t = arr ! x
>      lookup_prods _ = error "lookup_prods"

>      productions' = start_prods ++ concat rules2
>      prod_array  = listArray (0,length productions' - 1) productions'

>   return  (Grammar {
>               productions       = productions',
>               lookupProdNo      = (prod_array !),
>               lookupProdsOfName = lookup_prods,
>               token_specs       = tokspec,
>               terminals         = errorTok : catchTok : terminal_names,
>               non_terminals     = start_names ++ nonterm_names,
>                                       -- INCLUDES the %start tokens
>               starts            = zip4 parser_names start_names start_toks
>                                       start_partials,
>               types             = type_array,
>               token_names       = env_array,
>               first_nonterm     = first_nt,
>               first_term        = first_t,
>               eof_term          = last terminal_names,
>               priorities        = prios
>       },
>       Directives {
>               imported_identity                 = getImportedIdentity dirs,
>               monad             = getMonad dirs,
>               lexer             = getLexer dirs,
>               error_handler     = getError dirs,
>               error_expected    = getErrorExpectedMode dirs,
>               token_type        = getTokenType dirs,
>               expect            = getExpect dirs
>       })

Gofer-like stuff:

> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs =
>       combine [ (a,[b]) | (a,b) <- sortBy (comparing fst) xs]
>  where
>       combine [] = []
>       combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>       combine (a:r) = a : combine r


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
-- Check for every $i that i is <= the arity of the rule.

-- At the same time, we collect a list of the variables actually used in this
-- code, which is used by the backend.

> doCheckCode ::
>      Int                -- ^ Arity of the rule, i.e., maximal number of a variable.
>   -> String             -- ^ Haskell code for the semantic action of the rule.
>   -> M (String, [Int])  -- ^ The translated code and the variable occurrences (in reverse order).
> doCheckCode arity code0 = go code0 "" []
>   where go code acc used =
>           case code of
>               [] -> return (reverse acc, used)

>               '"'  :r    -> case reads code :: [(String,String)] of
>                                []       -> go r ('"':acc) used
>                                (s,r'):_ -> go r' (reverse (show s) ++ acc) used
>               a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc) used
>               '\'' :r    -> case reads code :: [(Char,String)] of
>                                []       -> go r  ('\'':acc) used
>                                (c,r'):_ -> go r' (reverse (show c) ++ acc) used
>               '\\':'$':r -> go r ('$':acc) used


Issue #335 (Andreas Abel, 2026-01-14):
Users often write '<$>' to mean '`fmap`', but Happy interprets it as '< $>' where '$>'
denotes the maximal variable (rightmost item).
Thus, reject '<$>' in semantic action code and suggest to either escape the '$'
or put a space after '<'.

>               c@'<':r@('$':'>':_) -> do
>                 addErr "Found '<$>' in semantic action code; either write '<\\$>' (for what usually is fmap) or '< $>' (for \"less than rightmost item\")"
>                 go r (c:acc) used

>               '$':'>':r -- the "rightmost token"
>                       | arity == 0 -> do addErr "$> in empty rule"
>                                          go r acc used
>                       | otherwise  -> go r (reverse (mkHappyVar arity) ++ acc)
>                                        (arity : used)

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
