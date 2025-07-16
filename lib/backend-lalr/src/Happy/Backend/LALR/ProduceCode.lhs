-----------------------------------------------------------------------------
The code generator.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Backend.LALR.ProduceCode (produceParser) where

> import Happy.Paths  ( version )
> import Data.Version              ( showVersion )
> import Happy.Grammar
> import Happy.Grammar.ExpressionWithHole ( substExpressionWithHole )
> import Happy.Tabular.LALR

> import Data.Maybe                ( isNothing, fromMaybe )
> import Data.Char                 ( ord, chr )
> import Data.List                 ( sortBy, nub )

> import Control.Monad.ST          ( ST, runST )
> import Data.Word
> import Data.Int
> import Data.Bits
> import Data.Array.ST             ( STUArray )
> import Data.Array.Unboxed        ( UArray )
> import Data.Array.MArray         ( MArray(..), freeze, readArray, writeArray )
> import Data.Array.IArray         ( Array, IArray(..), (!), array, assocs, elems )

%-----------------------------------------------------------------------------
Produce the complete output file.

> produceParser :: Grammar String               -- grammar info
>               -> Maybe AttributeGrammarExtras
>               -> Directives                   -- directives supplied in the .y-file
>               -> ActionTable                  -- action table
>               -> GotoTable                    -- goto table
>               -> [String]                     -- language extensions
>               -> Maybe String                 -- module header
>               -> Maybe String                 -- module trailer
>               -> Bool                         -- use coercions
>               -> Bool                         -- strict parser
>               -> String

> produceParser (Grammar
>               { productions = prods
>               , non_terminals = nonterms
>               , terminals = terms
>               , types = nt_types
>               , first_nonterm = first_nonterm'
>               , eof_term = eof
>               , first_term = fst_term
>               , token_names = token_names'
>               , token_specs = token_rep
>               , starts = starts'
>               })
>               mAg
>               (Directives
>               { lexer = lexer'
>               , imported_identity = imported_identity'
>               , monad = (use_monad,monad_context,monad_tycon,monad_then,monad_return)
>               , token_type = token_type'
>               , error_handler = error_handler'
>               , error_expected = error_expected'
>               })
>               action goto lang_exts module_header module_trailer
>               coerce strict
>     = ( top_opts
>       . maybestr module_header . nl
>       . str comment
>               -- comment goes *after* the module header, so that we
>               -- don't screw up any OPTIONS pragmas in the header.
>       . produceAbsSynDecl . nl
>       . produceTokToStringList
>       . produceActionTable
>       . produceReductions
>       . produceTokenConverter . nl
>       . produceIdentityStuff
>       . produceMonadStuff
>       . produceEntries
>       . produceStrict strict
>       . (case mAg of
>            Nothing -> id
>            Just ag -> produceAttributes ag)
>       . nl
>       . maybestr module_trailer . nl
>       ) ""
>  where
>    n_starts = length starts'
>    token = brack token_type'
>
>    nowarn_opts = str "{-# OPTIONS_GHC -w #-}" . nl
>       -- XXX Happy-generated code is full of warnings.  Some are easy to
>       -- fix, others not so easy, and others would require GHC version
>       -- #ifdefs.  For now I'm just disabling all of them.

We used to emit tabs for indentation, but since 2.0.0.1 we use 8 spaces for back-compat (#303):

>    indentStr = "        "
>    indent = str indentStr

>    intMaybeHash    = str "Happy_GHC_Exts.Int#"

>    -- Parsing monad and its constraints
>    pty = str monad_tycon                     -- str "P"
>    ptyAt a = brack' (pty . str " " . a)      -- \(str "a") -> str "(P a)"
>    pcont = str monad_context                 -- str "Read a", some constraint for "P" to be a monad

>    n_missing_types = length (filter isNothing (elems nt_types))
>    happyAbsSyn = str "(HappyAbsSyn " . str wild_tyvars . str ")"
>      where wild_tyvars = unwords (replicate n_missing_types "_")
>
>    top_opts =
>        nowarn_opts
>      . (str $ unlines
>           [ unwords [ "{-# LANGUAGE", l, "#-}" ] | l <- lang_exts ])

%-----------------------------------------------------------------------------
Make the abstract syntax type declaration, of the form:

data HappyAbsSyn a t1 .. tn
        = HappyTerminal a
        | HappyAbsSyn1 t1
        ...
        | HappyAbsSynn tn

>    produceAbsSynDecl

If we're using coercions, we need to generate the injections etc.

        data HappyAbsSyn ti tj tk ... = HappyAbsSyn

(where ti, tj, tk are type variables for the non-terminals which don't
 have type signatures).

        newtype HappyWrap<n> = HappyWrap<n> ti
        happyIn<n> :: ti -> HappyAbsSyn ti tj tk ...
        happyIn<n> x = unsafeCoerce# (HappyWrap<n> x)
        {-# INLINE happyIn<n> #-}

        happyOut<n> :: HappyAbsSyn ti tj tk ... -> tn
        happyOut<n> x = unsafeCoerce# x
        {-# INLINE happyOut<n> #-}

>     | coerce
>       = let
>             happy_item = str "HappyAbsSyn " . str_tyvars
>             bhappy_item = brack' happy_item
>
>             inject n ty
>               = (case ty of
>                   Nothing -> id
>                   Just tystr -> str "newtype " . mkHappyWrap n . str " = " . mkHappyWrap n . strspace . brack tystr . nl)
>               . mkHappyIn n . str " :: " . typeParam n ty
>               . str " -> " . bhappy_item . char '\n'
>               . mkHappyIn n . str " x = Happy_GHC_Exts.unsafeCoerce#" . strspace
>               . mkHappyWrapCon ty n (str "x")
>               . nl
>               . str "{-# INLINE " . mkHappyIn n . str " #-}"
>
>             extract n ty
>               = mkHappyOut n . str " :: " . bhappy_item
>               . str " -> " . typeParamOut n ty . char '\n'
>               . mkHappyOut n . str " x = Happy_GHC_Exts.unsafeCoerce# x\n"
>               . str "{-# INLINE " . mkHappyOut n . str " #-}"
>         in
>           str "newtype " . happy_item . str " = HappyAbsSyn HappyAny\n" -- see NOTE below
>         . interleave "\n" (map str
>           [ "#if __GLASGOW_HASKELL__ >= 607",
>             "type HappyAny = Happy_GHC_Exts.Any",
>             "#else",
>             "type HappyAny = forall a . a",
>             "#endif" ])
>         . interleave "\n"
>           [ inject n ty . nl . extract n ty | (n,ty) <- assocs nt_types ]
>         -- token injector
>         . str "happyInTok :: " . token . str " -> " . bhappy_item
>         . str "\nhappyInTok x = Happy_GHC_Exts.unsafeCoerce# x\n{-# INLINE happyInTok #-}\n"
>         -- token extractor
>         . str "happyOutTok :: " . bhappy_item . str " -> " . token
>         . str "\nhappyOutTok x = Happy_GHC_Exts.unsafeCoerce# x\n{-# INLINE happyOutTok #-}\n"

>         . str "\n"

NOTE: in the coerce case we always coerce all the semantic values to
HappyAbsSyn which is declared to be a synonym for Any.  This is the
type that GHC officially knows nothing about - it's the same type used
to implement Dynamic.  (in GHC 6.6 and older, Any didn't exist, so we
use the closest approximation namely forall a . a).

It's vital that GHC doesn't know anything about this type, because it
will use any knowledge it has to optimise, and if the knowledge is
false then the optimisation may also be false.  Previously we used (()
-> ()) as the type here, but this led to bogus optimisations (see GHC
ticket #1616).

Also, note that we must use a newtype instead of just a type synonym,
because the otherwise the type arguments to the HappyAbsSyn type
constructor will lose information.  See happy/tests/bug001 for an
example where this matters.

... Otherwise, output the declaration in full...

>     | otherwise
>       = str "data HappyAbsSyn " . str_tyvars
>       . str "\n" . indent . str "= HappyTerminal " . token
>       . str "\n" . indent . str "| HappyErrorToken Happy_Prelude.Int\n"
>       . interleave "\n"
>         [ str "" . indent . str "| " . makeAbsSynCon n . strspace . typeParam n ty
>         | (n, ty) <- assocs nt_types,
>           (nt_types_index ! n) == n]

>     where all_tyvars = [ 't' : show (getName n) | (n, Nothing) <- assocs nt_types ]
>           str_tyvars = str (unwords all_tyvars)

%-----------------------------------------------------------------------------
Next, the reduction functions.   Each one has the following form:

happyReduce_n_m = happyReduce n m reduction where {
   reduction (
        (HappyAbsSynX  | HappyTerminal) happy_var_1 :
        ..
        (HappyAbsSynX  | HappyTerminal) happy_var_q :
        happyRest)
         = HappyAbsSynY
                ( <<user supplied string>> ) : happyRest
        ; reduction _ _ = notHappyAtAll n m

where n is the non-terminal number, and m is the rule number.

NOTES on monad productions.  These look like

        happyReduce_275 = happyMonadReduce 0# 119# happyReduction_275
        happyReduction_275 (happyRest)
                =  happyThen (code) (\r -> happyReturn (HappyAbsSyn r))

why can't we pass the HappyAbsSyn constructor to happyMonadReduce and
save duplicating the happyThen/happyReturn in each monad production?
Because this would require happyMonadReduce to be polymorphic in the
result type of the monadic action, and since in array-based parsers
the whole thing is one recursive group, we'd need a type signature on
happyMonadReduce to get polymorphic recursion.  Sigh.

>    produceReductions =
>       interleave "\n\n"
>          (zipWith produceReduction (drop n_starts prods) [ n_starts .. ])

>    produceReduction (Production nt toks (code,vars_used) _) i

>     | is_monad_prod && (use_monad || imported_identity')
>       = mkReductionHdr (showInt lt) monad_reduce
>       . char '(' . interleave (" `HappyStk`\n" ++ indentStr) tokPatterns
>       . str "happyRest) tk\n" . indent . str " = happyThen ("
>       . str "("
>       . tokLets (char '(' . str code' . char ')')
>       . str ")"
>       . (if monad_pass_token then str " tk" else id)
>       . str "\n" . indent . str ") (\\r -> happyReturn (" . this_absSynCon . str " r))"

>     | specReduceFun lt
>       = mkReductionHdr id ("happySpecReduce_" ++ show lt)
>       . interleave ("\n" ++ indentStr) tokPatterns
>       . str " =  "
>       . tokLets (
>           this_absSynCon . str "\n" . indent . indent . str " "
>           . char '(' . str code' . str "\n" . indent . str ")"
>         )
>       . (if coerce || null toks || null vars_used then
>                 id
>          else
>                 nl . reductionFun . strspace
>               . interleave " " (replicate (length toks) (str "_"))
>               . str " = notHappyAtAll ")

>     | otherwise
>       = mkReductionHdr (showInt lt) "happyReduce"
>       . char '(' . interleave (" `HappyStk`\n" ++ indentStr) tokPatterns
>       . str "happyRest)\n" . indent . str " = "
>       . tokLets
>          ( this_absSynCon . str "\n" . indent . indent . str " "
>          . char '(' . str code'. str "\n" . indent . str ") `HappyStk` happyRest"
>          )

>       where
>               (code', is_monad_prod, monad_pass_token, monad_reduce)
>                     = case code of
>                         '%':'%':code1 -> (code1, True, True, "happyMonad2Reduce")
>                         '%':'^':code1 -> (code1, True, True, "happyMonadReduce")
>                         '%':code1     -> (code1, True, False, "happyMonadReduce")
>                         _ -> (code, False, False, "")

>               -- adjust the nonterminal number for the array-based parser
>               -- so that nonterminals start at zero.
>               adjusted_nt = getName nt - getName first_nonterm'

>               mkReductionHdr lt' s =
>                       let tysig = case lexer' of
>                             Nothing -> id
>                             _       -> mkReduceFun i . str " :: " . pcont
>                                      . str " => " . intMaybeHash
>                                      . str " -> " . str token_type'
>                                      . str " -> " . intMaybeHash
>                                      . str " -> Happy_IntList -> HappyStk "
>                                      . happyAbsSyn . str " -> "
>                                      . pty . str " " . happyAbsSyn . str "\n"
>                       in tysig . mkReduceFun i . str " = "
>                       . str s . strspace . lt' . strspace . showInt adjusted_nt
>                       . strspace . reductionFun . nl
>                       . reductionFun . strspace
>
>               reductionFun = str "happyReduction_" . shows i
>
>               tokPatterns
>                | coerce = reverse (map mkDummyVar [1 .. length toks])
>                | otherwise = reverse (zipWith tokPattern [1..] toks)
>
>               tokPattern n _ | n `notElem` vars_used = char '_'
>               tokPattern n t | t >= firstStartTok && t < fst_term
>                       = if coerce
>                               then mkHappyWrapCon (nt_types ! t) t (mkHappyVar n)
>                               else brack' (
>                                    makeAbsSynCon t . str "  " . mkHappyVar n
>                                    )
>               tokPattern n t
>                       = if coerce
>                               then mkHappyTerminalVar n t
>                               else str "(HappyTerminal "
>                                  . mkHappyTerminalVar n t
>                                  . char ')'
>
>               tokLets code''
>                  | coerce && not (null cases)
>                       = interleave ("\n"++indentStr) cases
>                       . code'' . str (replicate (length cases) '}')
>                  | otherwise = code''
>
>               cases = [ str "case " . extract t . strspace . mkDummyVar n
>                       . str " of { " . tokPattern n t . str " -> "
>                       | (n,t) <- zip [1..] toks,
>                         n `elem` vars_used ]
>
>               extract t | t >= firstStartTok && t < fst_term = mkHappyOut t
>                         | otherwise                     = str "happyOutTok"
>
>               lt = length toks

>               this_absSynCon | coerce    = mkHappyIn nt
>                              | otherwise = makeAbsSynCon nt

%-----------------------------------------------------------------------------
The token conversion function.

>    produceTokenConverter
>       = str "happyTerminalToTok term = case term of {\n" . indent
>       . (case lexer' of Just (_, eof') -> str eof' . str " -> " . eofTok . str ";\n" . indent; _ -> id)
>       . interleave (";\n" ++ indentStr) (map doToken token_rep)
>       . str "_ -> -1#;\n" . indent . str "}\n" -- token number -1# (INVALID_TOK) signals an invalid token
>       . str "{-# NOINLINE happyTerminalToTok #-}\n"
>       . str "\n" .
>       (case lexer' of {
>       Nothing ->
>         str "happyLex kend  _kmore []       = kend notHappyAtAll []\n"
>       . str "happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks\n"
>       . str "{-# INLINE happyLex #-}\n"
>       . str "\n"
>       . str "happyNewToken action sts stk = happyLex (\\tk -> " . eofAction "notHappyAtAll" . str ") ("
>             . str "\\i tk -> " . doAction . str " sts stk)\n"
>       . str "\n"
>       . str "happyReport " . eofTok . str " tk explist resume tks = happyReport' tks explist resume\n"
>       . str "happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\\tks -> resume (Happy_Prelude.tail tks))\n"
>             -- when the token is EOF, tk == _|_ (notHappyAtAll)
>             -- so we must not pass it to happyReport'
>       . str "\n";

>       Just (lexer'',eof') ->
>         str "happyLex kend kmore = " . str lexer'' . str " (\\tk -> case tk of {\n" . indent
>       . str eof' . str " -> kend tk;\n" . indent
>       . str "_ -> kmore (happyTerminalToTok tk) tk })\n"
>       . str "{-# INLINE happyLex #-}\n"
>       . str "\n"
>       . str "happyNewToken action sts stk = happyLex (\\tk -> " . eofAction "tk" . str ") ("
>             . str "\\i tk -> " . doAction . str " sts stk)\n"
>       . str "\n"
>       . str "happyReport " . eofTok . str " = happyReport'\n"
>       . str "happyReport _ = happyReport'\n"
>             -- superfluous pattern match needed to force happyReport to
>             -- have the correct type.
>       . str "\n";
>       })

>       where

>         eofAction tk = str "happyDoAction "
>                      . eofTok . strspace . str tk
>                      . str " action sts stk"
>         eofTok = showInt (tokIndex eof)
>         doAction = str "happyDoAction i tk action"
>         doToken (i,tok) = str (removeDollarDollar tok) . str " -> " . showInt (tokIndex i)

Use a variable rather than '_' to replace '$$', so we can use it on
the left hand side of '@'.

>         removeDollarDollar tok = case tok of
>              TokenFixed t -> t
>              TokenWithValue e -> substExpressionWithHole e "happy_dollar_dollar"

>    mkHappyTerminalVar :: Int -> Name -> String -> String
>    mkHappyTerminalVar i t =
>     case lookup t token_rep of
>       Nothing -> pat
>       Just (TokenFixed _) -> pat
>       Just (TokenWithValue e) -> brack $ substExpressionWithHole e $ pat []
>     where
>         pat = mkHappyVar i
>    tokIndex i = getName i - getName fst_term + 2 -- +2: errorTok, catchTok

%-----------------------------------------------------------------------------
Action Tables.

Here we do a bit of trickery and replace the normal default action
(failure) for each state with at least one reduction action.  For each
such state, we pick one reduction action to be the default action.
This should make the code smaller without affecting the speed.
It changes the sematics for errors, however; errors could be detected in a
different state now (but they'll still be detected at the same point in the
token stream).

SG: For a data point, in issue93 the happyTable triples in size when we always
pick failure as the default reduction.
Presumably that is because there are quite a few reduction states, in which the
only non-default transition is a reduction.
Our scheme above ensures that these states don't occupy space in the main
happyTable at all; they just get an entry in the happyDefActions.

Further notes on default cases:

Default reductions are important when error recovery is considered: we
don't allow reductions whilst in error recovery, so we'd like the
parser to automatically reduce down to a state where the error token
can be shifted before entering error recovery.  This is achieved by
using default reductions wherever possible.

One case to consider is:

State 345

        con -> conid .                                      (rule 186)
        qconid -> conid .                                   (rule 212)

        error          reduce using rule 212
        '{'            reduce using rule 186
        etc.

we should make reduce_212 the default reduction here.  So the rules become:

   * if there is a production
        error -> reduce_n
     then make reduce_n the default action.
   * if there is a non-reduce action for the error token, the default action
     for this state must be "fail".
   * otherwise pick the most popular reduction in this state for the default.
   * if there are no reduce actions in this state, then the default
     action remains 'enter error recovery'.

This gives us an invariant: there won't ever be a production of the
type 'error -> reduce_n' explicitly in the grammar, which means that
whenever an unexpected token occurs, either the parser will reduce
straight back to a state where the error token can be shifted, or if
none exists, we'll get a parse error.  In theory, we won't need the
machinery to discard states in the parser...

>    produceActionTable
>       = produceActionArray
>       . produceReduceArray
>       . produceRuleArray
>       . produceCatchStates
>       . str "happy_n_terms = " . shows n_terminals . str " :: Happy_Prelude.Int\n"
>       . str "happy_n_nonterms = " . shows n_nonterminals . str " :: Happy_Prelude.Int\n\n"
>       . str "happy_n_starts = " . shows n_starts . str " :: Happy_Prelude.Int\n\n"
>
>    produceTokToStringList
>       = str "{-# NOINLINE happyTokenStrings #-}\n"
>       . str "happyTokenStrings = " . shows (drop (getName fst_term - 1) (elems token_names')) . str "\n"
>                                            -- fst_term - 1: fst_term includes eofToken, but that is last in the list.
>       . str "\n"

action array indexed by (terminal * last_state) + state

>    produceActionArray
>           = str "happyActOffsets :: HappyAddr\n"
>           . str "happyActOffsets = HappyA# \"" --"
>           . hexChars act_offs
>           . str "\"#\n\n" --"
>
>           . str "happyGotoOffsets :: HappyAddr\n"
>           . str "happyGotoOffsets = HappyA# \"" --"
>           . hexChars goto_offs
>           . str "\"#\n\n"  --"
>
>           . str "happyDefActions :: HappyAddr\n"
>           . str "happyDefActions = HappyA# \"" --"
>           . hexChars defaults
>           . str "\"#\n\n" --"
>
>           . str "happyCheck :: HappyAddr\n"
>           . str "happyCheck = HappyA# \"" --"
>           . hexChars check
>           . str "\"#\n\n" --"
>
>           . str "happyTable :: HappyAddr\n"
>           . str "happyTable = HappyA# \"" --"
>           . hexChars table
>           . str "\"#\n\n" --"

>    n_terminals = length terms
>    n_nonterminals = length nonterms - n_starts -- lose %starts
>
>    (act_offs,goto_offs,table,defaults,check,catch_states)
>       = mkTables action goto first_nonterm' fst_term
>               n_terminals n_nonterminals n_starts
>
>    produceReduceArray
>       = str "happyReduceArr = Happy_Data_Array.array ("
>               . shows (n_starts :: Int) -- omit the %start reductions
>               . str ", "
>               . shows n_rules
>               . str ") [\n"
>       . interleave' ",\n" (map reduceArrElem [n_starts..n_rules])
>       . str "\n" . indent . str "]\n\n"
>
>    produceRuleArray -- rule number to (non-terminal number, rule length)
>       = str "happyRuleArr :: HappyAddr\n"
>       . str "happyRuleArr = HappyA# \"" -- "
>       . hexChars (concatMap (\(nt,len) -> [nt,len]) ruleArrElems)
>       . str "\"#\n\n" --"
>
>    ruleArrElems = map (\(Production nt toks _code _prio) -> (getName nt - getName first_nonterm',length toks)) (drop n_starts prods)
>
>    n_rules = length prods - 1 :: Int
>
>    produceCatchStates
>       = str "happyCatchStates :: [Happy_Prelude.Int]\n"
>       . str "happyCatchStates = " . shows catch_states . str "\n\n"

>    showInt i = shows i . showChar '#'

This lets examples like:

        data HappyAbsSyn t1
                = HappyTerminal ( HaskToken )
                | HappyAbsSyn1 (  HaskExp  )
                | HappyAbsSyn2 (  HaskExp  )
                | HappyAbsSyn3 t1

*share* the definition for ( HaskExp )

        data HappyAbsSyn t1
                = HappyTerminal ( HaskToken )
                | HappyAbsSyn1 (  HaskExp  )
                | HappyAbsSyn3 t1

... cutting down on the work that the type checker has to do.

Note, this *could* introduce lack of polymophism,
for types that have alphas in them. Maybe we should
outlaw them inside { }

>    nt_types_index :: Array Name Name
>    nt_types_index = array (bounds nt_types)
>                       [ (a, fn a b) | (a, b) <- assocs nt_types ]
>     where
>       fn n Nothing = n
>       fn _ (Just a) = fromMaybe (error "can't find an item in list") (lookup a assoc_list)
>       assoc_list = [ (b,a) | (a, Just b) <- assocs nt_types ]

>    makeAbsSynCon = mkAbsSynCon nt_types_index


>    produceIdentityStuff | use_monad = id
>     | imported_identity' =
>            str "type HappyIdentity = Identity\n"
>          . str "happyIdentity = Identity\n"
>          . str "happyRunIdentity = runIdentity\n\n"
>     | otherwise =
>            str "newtype HappyIdentity a = HappyIdentity a\n"
>          . str "happyIdentity = HappyIdentity\n"
>          . str "happyRunIdentity (HappyIdentity a) = a\n\n"
>          . str "instance Happy_Prelude.Functor HappyIdentity where\n"
>          . str "    fmap f (HappyIdentity a) = HappyIdentity (f a)\n\n"
>          . str "instance Applicative HappyIdentity where\n"
>          . str "    pure  = HappyIdentity\n"
>          . str "    (<*>) = ap\n"
>          . str "instance Happy_Prelude.Monad HappyIdentity where\n"
>          . str "    return = pure\n"
>          . str "    (HappyIdentity p) >>= q = q p\n\n"

MonadStuff:

  - with no %monad or %lexer:

        happyThen    :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
        happyReturn  :: () => a -> HappyIdentity a
        happyThen1   m k tks = happyThen m (\a -> k a tks)
        happyFmap1   f m tks = happyThen (m tks) (\a -> happyReturn (f a))
        happyReturn1 = \a tks -> happyReturn a

  - with %monad:

        happyThen    :: CONTEXT => P a -> (a -> P b) -> P b
        happyReturn  :: CONTEXT => a -> P a
        happyThen1   m k tks = happyThen m (\a -> k a tks)
        happyFmap1   f m tks = happyThen (m tks) (\a -> happyReturn (f a))
        happyReturn1 = \a tks -> happyReturn a

  - with %monad & %lexer:

        happyThen    :: CONTEXT => P a -> (a -> P b) -> P b
        happyReturn  :: CONTEXT => a -> P a
        happyThen1   = happyThen
        happyReturn1 = happyReturn
        happyFmap1 f m = happyThen m (\a -> happyReturn (f a))


>    produceMonadStuff =
>            str "happyThen :: " . pcont . str " => " . ptyAt (str "a")
>          . str " -> (a -> " . ptyAt (str "b")
>          . str ") -> " . ptyAt (str "b") . str "\n"
>          . str "happyThen = " . brack monad_then . nl
>          . str "happyReturn :: " . pcont . str " => a -> " . ptyAt (str "a") . str "\n"
>          . str "happyReturn = " . brack monad_return . nl
>          . case lexer' of
>               Nothing ->
>                  str "happyThen1 m k tks = (" . str monad_then
>                . str ") m (\\a -> k a tks)\n"
>                . str "happyFmap1 f m tks = happyThen (m tks) (\\a -> happyReturn (f a))\n"
>                . str "happyReturn1 :: " . pcont . str " => a -> b -> " . ptyAt (str "a") . str "\n"
>                . str "happyReturn1 = \\a tks -> " . brack monad_return
>                . str " a\n"
>                . str "happyReport' :: " . pcont . str " => "
>                . str "[" . token . str "] -> "
>                . str "[Happy_Prelude.String] -> ("
>                . str "[" . token . str "] -> "
>                . ptyAt (str "a") . str ") -> "
>                . ptyAt (str "a")
>                . str "\n"
>                . str "happyReport' = " . callReportError . str "\n"
>                . str "\n"
>                . str "happyAbort :: " . pcont . str " => "
>                . str "[" . token . str "] -> "
>                . ptyAt (str "a")
>                . str "\n"
>                . str "happyAbort = " . str abort_handler . str "\n"
>                . str "\n"
>               _ ->
>                let
>                  happyParseSig =
>                        str "happyParse :: " . pcont . str " => " . intMaybeHash
>                      . str " -> " . pty . str " " . happyAbsSyn . str "\n"
>                      . str "\n"
>                  newTokenSig =
>                        str "happyNewToken :: " . pcont . str " => " . intMaybeHash
>                      . str " -> Happy_IntList -> HappyStk " . happyAbsSyn
>                      . str " -> " . ptyAt happyAbsSyn . str"\n"
>                      . str "\n"
>                  doActionSig =
>                        str "happyDoAction :: " . pcont . str " => " . intMaybeHash
>                      . str " -> " . str token_type' . str " -> " . intMaybeHash
>                      . str " -> Happy_IntList -> HappyStk " . happyAbsSyn
>                      . str " -> " . ptyAt happyAbsSyn . str "\n"
>                      . str "\n"
>                  reduceArrSig =
>                        str "happyReduceArr :: " . pcont
>                      . str " => Happy_Data_Array.Array Happy_Prelude.Int (" . intMaybeHash
>                      . str " -> " . str token_type' . str " -> " . intMaybeHash
>                      . str " -> Happy_IntList -> HappyStk " . happyAbsSyn
>                      . str " -> " . ptyAt happyAbsSyn . str ")\n"
>                      . str "\n"
>                  in happyParseSig . newTokenSig . doActionSig . reduceArrSig
>                . str "happyThen1 :: " . pcont . str " => " . pty
>                . str " a -> (a -> "  . pty
>                . str " b) -> " . pty . str " b\n"
>                . str "happyThen1 = happyThen\n"
>                . str "happyFmap1 f m = happyThen m (\\a -> happyReturn (f a))\n"
>                . str "happyReturn1 :: " . pcont . str " => a -> " . ptyAt (str "a") . str "\n"
>                . str "happyReturn1 = happyReturn\n"
>                . str "happyReport' :: " . pcont . str " => "
>                . token . str " -> "
>                . str "[Happy_Prelude.String] -> "
>                . ptyAt (str "a") . str " -> "
>                . ptyAt (str "a")
>                . str "\n"
>                . str "happyReport' = " . callReportError . str "\n"
>                . str "\n"
>                . str "happyAbort :: " . pcont . str " => "
>                . ptyAt (str "a")
>                . str "\n"
>                . str "happyAbort = " . str abort_handler . str "\n"
>                . str "\n"

The error handler takes up to three arguments.
An error handler specified with %error is passed the current token
when used with %lexer as the first argument, but happyError (the old way but kept for
compatibility) is not passed the current token.
Furthermore, the second argument is the list of expected tokens
in the presence of the %error.expected directive.
The last argument is the "resumption", a continuation that tries to find
an item on the stack taking a @catch@ terminal where parsing may resume,
in the presence of the two-argument form of the %error directive.
In order to support the legacy %errorhandlertype directive, we retain
have a special code path for `OldExpected`.

>    callReportError = -- this one wraps around report_error_handler to expose a unified interface
>       str "(\\tokens expected resume -> " .
>       (if use_monad then str ""
>                     else str "HappyIdentity Happy_Prelude.$ ") .
>       report_error_handler .
>       (case error_expected' of
>          OldExpected -> str " (tokens, expected)"  -- back-compat for %errorhandlertype
>          _ ->
>            (case (error_handler', lexer') of (DefaultErrorHandler, Just _) -> id
>                                              _                             -> str " tokens") .
>            (case error_expected' of NewExpected -> str " expected"
>                                     NoExpected  -> id)) .
>       (case error_handler' of ResumptiveErrorHandler{} -> str " resume"
>                               _                        -> id) .
>       str ")"
>    report_error_handler = case error_handler' of
>       DefaultErrorHandler                  -> str "happyError"
>       CustomErrorHandler h                 -> brack h
>       ResumptiveErrorHandler _abort report -> brack report
>    abort_handler = case error_handler' of
>       ResumptiveErrorHandler abort _report -> abort
>       _                                    -> "Happy_Prelude.error \"Called abort handler in non-resumptive parser\""

>    reduceArrElem n
>      = str "" . indent . str "(" . shows n . str " , "
>      . str "happyReduce_" . shows n . char ')'

-----------------------------------------------------------------------------
-- Produce the parser entry and exit points

>    produceEntries
>       = interleave "\n\n" (map produceEntry (zip starts' [0..]))
>       . case mAg of
>           Nothing -> id
>           Just ag -> produceAttrEntries ag starts'

>    produceEntry :: ((String, t0, Name, t1), Int) -> String -> String
>    produceEntry ((name, _start_nonterm, accept_nonterm, _partial), no)
>       = (if isNothing mAg then str name else str "do_" . str name)
>       . maybe_tks
>       . str " = "
>       . str unmonad
>       . str "happySomeParser where\n"
>       . str " happySomeParser = happyThen (happyParse "
>       . shows no . str "#"
>       . maybe_tks
>       . str ") "
>       . brack' (if coerce
>                    then str "\\x -> happyReturn (let {" . mkHappyWrapCon (nt_types ! accept_nonterm) accept_nonterm (str "x'")
>                       . str " = " . mkHappyOut accept_nonterm . str " x} in x')"
>                    else str "\\x -> case x of {HappyAbsSyn"
>                       . showsName (nt_types_index ! accept_nonterm)
>                       . str " z -> happyReturn z; _other -> notHappyAtAll }"
>                )
>     where
>       maybe_tks | isNothing lexer' = str " tks"
>                 | otherwise = id
>       unmonad | use_monad = ""
>                 | otherwise = "happyRunIdentity "

>    produceAttrEntries ag starts''
>       = interleave "\n\n" (map f starts'')
>     where
>       f = case (use_monad,lexer') of
>             (True,Just _)  -> \(name,_,_,_) -> monadAndLexerAE name
>             (True,Nothing) -> \(name,_,_,_) -> monadAE name
>             (False,Just _) -> error "attribute grammars not supported for non-monadic parsers with %lexer"
>             (False,Nothing)-> \(name,_,_,_) -> regularAE name
>
>       defaultAttr = fst (head $ attributes ag)
>
>       monadAndLexerAE name
>         = str name . str " = "
>         . str "do { "
>         . str "f <- do_" . str name . str "; "
>         . str "let { (conds,attrs) = f happyEmptyAttrs } in do { "
>         . str "Happy_Prelude.sequence_ conds; "
>         . str "Happy_Prelude.return (". str defaultAttr . str " attrs) }}"
>       monadAE name
>         = str name . str " toks = "
>         . str "do { "
>         . str "f <- do_" . str name . str " toks; "
>         . str "let { (conds,attrs) = f happyEmptyAttrs } in do { "
>         . str "Happy_Prelude.sequence_ conds; "
>         . str "Happy_Prelude.return (". str defaultAttr . str " attrs) }}"
>       regularAE name
>         = str name . str " toks = "
>         . str "let { "
>         . str "f = do_" . str name . str " toks; "
>         . str "(conds,attrs) = f happyEmptyAttrs; "
>         . str "x = Happy_Prelude.foldr Happy_GHC_Exts.seq attrs conds; "
>         . str "} in (". str defaultAttr . str " x)"

----------------------------------------------------------------------------
-- Produce attributes declaration for attribute grammars

> produceAttributes :: AttributeGrammarExtras -> String -> String
> produceAttributes AttributeGrammarExtras {
>         attributes = attrs,
>         attributetype = attributeType
>     }
>     = str "data " . attrHeader . str " = HappyAttributes {" . attributes' . str "}" . nl
>     . str "happyEmptyAttrs = HappyAttributes {" . attrsErrors . str "}" . nl

>   where attributes'  = foldl1 (\x y -> x . str ", " . y) $ map formatAttribute attrs
>         formatAttribute (ident,typ) = str ident . str " :: " . str typ
>         attrsErrors = foldl1 (\x y -> x . str ", " . y) $ map attrError attrs
>         attrError (ident,_) = str ident . str " = Happy_Prelude.error \"invalid reference to attribute '" . str ident . str "'\""
>         attrHeader =
>             case attributeType of
>             [] -> str "HappyAttributes"
>             _  -> str attributeType


-----------------------------------------------------------------------------
-- Strict or non-strict parser

> produceStrict :: Bool -> String -> String
> produceStrict strict
>       | strict    = str "happySeq = happyDoSeq\n\n"
>       | otherwise = str "happySeq = happyDontSeq\n\n"

-----------------------------------------------------------------------------
Replace all the $n variables with happy_vars, and return a list of all the
vars used in this piece of code.

> actionVal :: LRAction -> Int
> actionVal (LR'Shift  state _) = state + 1
> actionVal (LR'Reduce rule _)  = -(rule + 1)
> actionVal LR'Accept           = -1
> actionVal (LR'Multiple _ a)   = actionVal a
> actionVal LR'Fail             = 0
> actionVal LR'MustFail         = 0

See notes under "Action Tables" above for some subtleties in this function.

> getDefault :: [(Name, LRAction)] -> LRAction
> getDefault actions
>   -- pick out the action for the error token, if any
>   | (act : _) <- error_acts, act /= LR'Fail
>   = case act of
>
>       -- use error reduction as the default action, if there is one.
>       LR'Reduce _ _                 -> act
>       LR'Multiple _ (LR'Reduce _ _) -> act
>
>       -- if the error token is shifted or otherwise, don't generate
>       -- a default reduction action.  This is *important*!
>       _ -> LR'Fail
>
>   -- do not reduce by default in a state that could shift the catch token.
>   -- otherwise upon an error, we discard viable resumption points from the
>   -- parsing stack.
>   -- This makes a difference on GHC's parser for input such as
>   --    f = foo data; x = + blah
>   -- where we must detect `data` as a parse error early enough to parse
>   -- `foo data` as an application
>   | (LR'Shift{} : _) <- catch_acts
>   = LR'Fail
>   | (LR'Multiple _ LR'Shift{} : _) <- catch_acts
>   = LR'Fail
>
>   | otherwise
>   -- no error or catch actions, pick a reduce to be the default.
>   = case reduces of
>       _ -> case reduces of
>         [] -> LR'Fail
>         (act:_) -> act    -- pick the first one we see for now
>
>   where
>     error_acts = [ act | (e, act) <- actions, e == errorTok ]
>     catch_acts = [ act | (e, act) <- actions, e == catchTok ]
>     reduces
>           =  [ act | (_, act@(LR'Reduce _ _)) <- actions ]
>           ++ [ act | (_, LR'Multiple _ act@(LR'Reduce _ _)) <- actions ]

-----------------------------------------------------------------------------
-- Generate packed parsing tables.

-- happyActOff ! state
--     Offset within happyTable of actions for state

-- happyGotoOff ! state
--     Offset within happyTable of gotos for state

-- happyTable
--      Combined action/goto table

-- happyDefAction ! state
--      Default action for state

-- happyCheck
--      Indicates whether we should use the default action for state

-- the table is laid out such that the action for a given state & token
-- can be found by:
--
--        off    = happyActOff ! state
--        off_i  = off + token
--        check  | off_i => 0 = (happyCheck ! off_i) == token
--               | otherwise  = False
--        action | check      = happyTable ! off_i
--               | otherwise  = happyDefAaction ! off_i


-- figure out the default action for each state.  This will leave some
-- states with no *real* actions left.

-- for each state with one or more real actions, sort states by
-- width/spread of tokens with real actions, then by number of
-- elements with actions, so we get the widest/densest states
-- first. (I guess the rationale here is that we can use the
-- thin/sparse states to fill in the holes later, and also we
-- have to do less searching for the more complicated cases).

-- try to pair up states with identical sets of real actions.

-- try to fit the actions into the check table, using the ordering
-- from above.

SG: If you want to know more about similar compression schemes, consult
      Storing a Sparse Table (https://dl.acm.org/doi/10.1145/359168.359175)
One can think of the mapping @\(state,token) -> (offs ! state)+token@ as a hash
and @check@ as the way to detect "collisions" (i.e., default entries).

> mkTables
>        :: ActionTable -> GotoTable -> Name -> Name -> Int -> Int -> Int ->
>        ( [Int]         -- happyActOffsets
>        , [Int]         -- happyGotoOffsets
>        , [Int]         -- happyTable
>        , [Int]         -- happyDefAction
>        , [Int]         -- happyCheck
>        , [Int]         -- happyCatchStates
>        )
>
> mkTables action goto first_nonterm' fst_term
>               n_terminals n_nonterminals n_starts
>
>  = ( elems act_offs
>    , elems goto_offs
>    , take max_off (elems table)
>    , def_actions
>    , take max_off (elems check)
>    , shifted_catch_states
>    )
>  where
>
>        (table,check,act_offs,goto_offs,max_off)
>                = runST (genTables (length actions)
>                         max_token
>                         sorted_actions)
>
>        -- the maximum token number used in the parser
>        max_token = max n_terminals (n_starts+n_nonterminals) - 1
>
>        def_actions = map (\(_,_,def,_,_,_) -> def) actions
>
>        actions :: [TableEntry]
>        actions =
>                [ (ActionEntry,
>                   state,
>                   actionVal default_act,
>                   if null acts'' then 0
>                        else fst (last acts'') - fst (head acts''),
>                   length acts'',
>                   acts'')
>                | (state, acts) <- assocs action,
>                  let (err:catch:_dummy:vec) = assocs acts
>                      vec' = drop (n_starts+n_nonterminals) vec
>                      acts' = filter notFail (err:catch:vec')
>                      default_act = getDefault acts'
>                      acts'' = mkActVals acts' default_act
>                ]
>
>        shifted_catch_states :: [Int]
>        shifted_catch_states = -- collect the states in which we have just shifted a catchTok
>          nub [ to_state | (_from_state, acts) <- assocs action
>                         , let (_err:catch:_) = assocs acts
>                         , (_tok, LR'Shift to_state _) <- return catch ]
>
>        -- adjust terminals by -(fst_term+2), so they start at 2 (error is 0, catch is 1).
>        --  (see ARRAY_NOTES)
>        adjust :: Name -> Int
>        adjust token | token == errorTok = 0
>                     | token == catchTok = 1
>                     | otherwise         = getName token - getName fst_term + 2
>
>        mkActVals :: [(Name, LRAction)] -> LRAction -> [(Int, Int)]
>        mkActVals assocs' default_act =
>                [ (adjust token, actionVal act)
>                | (token, act) <- assocs'
>                , act /= default_act ]
>
>        gotos :: [TableEntry]
>        gotos = [ (GotoEntry,
>                   state, 0,
>                   if null goto_vals then 0
>                        else fst (last goto_vals) - fst (head goto_vals),
>                   length goto_vals,
>                   goto_vals
>                  )
>                | (state, goto_arr) <- assocs goto,
>                let goto_vals = mkGotoVals (assocs goto_arr)
>                ]
>
>        -- adjust nonterminals by -first_nonterm', so they start at zero
>        --  (see ARRAY_NOTES)
>        mkGotoVals assocs' =
>                [ (getName token - getName first_nonterm', i) | (token, Goto i) <- assocs' ]
>
>        sorted_actions = sortBy (flip cmp_state) (actions ++ gotos)
>        cmp_state (_,_,_,width1,tally1,_) (_,_,_,width2,tally2,_)
>                | width1 < width2  = LT
>                | width1 == width2 = compare tally1 tally2
>                | otherwise = GT

> data ActionOrGoto = ActionEntry | GotoEntry
> type TableEntry = ( ActionOrGoto
>                   , Int {-stateno-}
>                   , Int {-default-}
>                   , Int {-width-}
>                   , Int {-tally-}
>                   , [(Int,Int)]
>                   )

> genTables
>        :: Int                         -- number of actions
>        -> Int                         -- maximum token no.
>        -> [TableEntry]                -- entries for the table
>        -> ST s ( UArray Int Int       -- table
>                , UArray Int Int       -- check
>                , UArray Int Int       -- action offsets
>                , UArray Int Int       -- goto offsets
>                , Int                  -- highest offset in table
>                )
>
> genTables n_actions max_token entries = do
>
>   table      <- newArray (0, mAX_TABLE_SIZE) 0
>   check      <- newArray (0, mAX_TABLE_SIZE) (-1)
>   act_offs   <- newArray (0, n_actions) 0
>   goto_offs  <- newArray (0, n_actions) 0
>   off_arr    <- newArray (-max_token, mAX_TABLE_SIZE) 0
>
>   max_off    <- genTables' table check act_offs goto_offs off_arr entries
>                          max_token
>
>   table'     <- freeze table
>   check'     <- freeze check
>   act_offs'  <- freeze act_offs
>   goto_offs' <- freeze goto_offs
>   return (table',check',act_offs',goto_offs',max_off+1)

>   where
>        n_states = n_actions - 1
>        mAX_TABLE_SIZE = n_states * (max_token + 1)


> genTables'
>        :: STUArray s Int Int          -- table
>        -> STUArray s Int Int          -- check
>        -> STUArray s Int Int          -- action offsets
>        -> STUArray s Int Int          -- goto offsets
>        -> STUArray s Int Int          -- offset array
>        -> [TableEntry]                -- entries for the table
>        -> Int                         -- maximum token no.
>        -> ST s Int                    -- highest offsets in table
>
> genTables' table check act_offs goto_offs off_arr entries
>            max_token
>       = fit_all entries 0 1
>   where
>
>        fit_all [] max_off _ = return max_off
>        fit_all (s:ss) max_off fst_zero = do
>          (off, new_max_off, new_fst_zero) <- fit s max_off fst_zero
>          ss' <- same_states s ss off
>          writeArray off_arr off 1
>          fit_all ss' new_max_off new_fst_zero
>
>        -- try to merge identical states.  We only try the next state(s)
>        -- in the list, but the list is kind-of sorted so we shouldn't
>        -- miss too many.
>        same_states _ [] _ = return []
>        same_states s@(_,_,_,_,_,acts) ss@((e,no,_,_,_,acts'):ss') off
>          | acts == acts' = do writeArray (which_off e) no off
>                               same_states s ss' off
>          | otherwise = return ss
>
>        which_off ActionEntry = act_offs
>        which_off GotoEntry   = goto_offs
>
>        -- fit a vector into the table.  Return the offset of the vector,
>        -- the maximum offset used in the table, and the offset of the first
>        -- entry in the table (used to speed up the lookups a bit).
>        fit (_,_,_,_,_,[]) max_off fst_zero = return (0,max_off,fst_zero)
>
>        fit (act_or_goto, state_no, _deflt, _, _, state@((t,_):_))
>           max_off fst_zero = do
>                -- start at offset 1 in the table: all the empty states
>                -- (states with just a default reduction) are mapped to
>                -- offset zero.
>          off <- findFreeOffset (-t+fst_zero) check off_arr state
>          let new_max_off | furthest_right > max_off = furthest_right
>                          | otherwise                = max_off
>              furthest_right = off + max_token
>
>          -- trace ("fit: state " ++ show state_no ++ ", off " ++ show off ++ ", elems " ++ show state) $ do
>
>          writeArray (which_off act_or_goto) state_no off
>          addState off table check state
>          new_fst_zero <- findFstFreeSlot check fst_zero
>          return (off, new_max_off, new_fst_zero)

When looking for a free offset in the table, we use the 'check' table
rather than the main table.  The check table starts off with (-1) in
every slot, because that's the only thing that doesn't overlap with
any tokens (non-terminals start at 0, terminals start at 1).

Because we use 0 for LR'MustFail as well as LR'Fail, we can't check
for free offsets in the main table because we can't tell whether a
slot is free or not.

> -- Find a valid offset in the table for this state.
> findFreeOffset :: Int -> STUArray s Int Int -> STUArray s Int Int -> [(Int, Int)] -> ST s Int
> findFreeOffset off table off_arr state = do
>     -- offset 0 isn't allowed
>   if off == 0 then try_next else do
>
>     -- don't use an offset we've used before
>   b <- readArray off_arr off
>   if b /= 0 then try_next else do
>
>     -- check whether the actions for this state fit in the table
>   ok <- fits off state table
>   if not ok then try_next else return off
>  where
>       try_next = findFreeOffset (off+1) table off_arr state


> fits :: Int -> [(Int,Int)] -> STUArray s Int Int -> ST s Bool
> fits _   []           _     = return True
> fits off ((t,_):rest) table = do
>   i <- readArray table (off+t)
>   if i /= -1 then return False
>              else fits off rest table

> addState :: Int -> STUArray s Int Int -> STUArray s Int Int -> [(Int, Int)]
>          -> ST s ()
> addState _   _     _     [] = return ()
> addState off table check ((t,val):state) = do
>    writeArray table (off+t) val
>    writeArray check (off+t) t
>    addState off table check state

> notFail :: (Name, LRAction) -> Bool
> notFail (_, LR'Fail) = False
> notFail _           = True

> findFstFreeSlot :: STUArray s Int Int -> Int -> ST s Int
> findFstFreeSlot table n = do
>        i <- readArray table n
>        if i == -1 then return n
>                   else findFstFreeSlot table (n+1)

-----------------------------------------------------------------------------
-- Misc.

> showsName :: Name -> ShowS
> showsName = shows . getName

> comment :: String
> comment =
>         "-- parser produced by Happy Version " ++ showVersion version ++ "\n\n"

> mkAbsSynCon :: Array Name Name -> Name -> String -> String
> mkAbsSynCon fx t      = str "HappyAbsSyn"   . showsName (fx ! t)

> mkHappyVar, mkReduceFun, mkDummyVar :: Int -> String -> String
> mkHappyVar n          = str "happy_var_"    . shows n
> mkReduceFun n         = str "happyReduce_"  . shows n
> mkDummyVar n          = str "happy_x_"      . shows n

> mkHappyWrap :: Name -> String -> String
> mkHappyWrap n = str "HappyWrap" . showsName n

> mkHappyWrapCon :: Maybe a -> Name -> (String -> String) -> String -> String
> mkHappyWrapCon Nothing  _ s = s
> mkHappyWrapCon (Just _) n s = brack' (mkHappyWrap n . strspace . s)

> mkHappyIn, mkHappyOut :: Name -> String -> String
> mkHappyIn n           = str "happyIn"  . showsName n
> mkHappyOut n          = str "happyOut" . showsName n

> typeParam, typeParamOut :: Name -> Maybe String -> ShowS
> typeParam n Nothing   = char 't' . showsName n
> typeParam _ (Just ty) = brack ty
> typeParamOut n Nothing  = char 't' . showsName n
> typeParamOut n (Just _) = mkHappyWrap n

> specReduceFun :: Int -> Bool
> specReduceFun = (<= 3)

-------------------------------------------------------------------------------
-- Fast string-building functions.

> str :: String -> String -> String
> str = showString
> char :: Char -> String -> String
> char c = (c :)
> interleave :: String -> [String -> String] -> String -> String
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' :: String -> [String -> String] -> String -> String
> interleave' s = foldr1 (\a b -> a . str s . b)

> strspace :: String -> String
> strspace = char ' '
> nl :: String -> String
> nl = char '\n'

> maybestr :: Maybe String -> String -> String
> maybestr (Just s)     = str s
> maybestr _            = id

> brack :: String -> String -> String
> brack s = str ('(' : s) . char ')'
> brack' :: (String -> String) -> String -> String
> brack' s = char '(' . s . char ')'

-----------------------------------------------------------------------------
-- Convert an integer to a 32-bit number encoded in little-endian
-- \xNN\xNN\xNN\xNN format suitable for placing in a string.

> hexChars :: [Int] -> String -> String
> hexChars is s = foldr (hexChar . toInt32) s is

The following definition of @hexChar@ chooses a little endian encoding for `Int32` .
Ergo, the compiled parser must use the same endianness when decoding array entries.
On big endian architectures, this means users will have to compile with `WORDS_BIGENDIAN`,
which is defined in the GHC provided C header `MachDeps.h`.

> hexChar :: Int32 -> String -> String
> hexChar i s = foldr (toHex . byte i) s [0,1,2,3]

> byte :: Int32 -> Int -> Word8
> byte n i = fromIntegral (0xFF .&. shiftR n (i*8))

> toHex :: Word8 -> String -> String
> toHex i s = '\\':'x':hexDig (0xF .&. shiftR i 4):hexDig (0xF .&. i):s

> hexDig :: Word8 -> Char
> hexDig i | i <= 9    = chr (fromIntegral i + ord '0')
>          | otherwise = chr (fromIntegral i - 10 + ord 'a')

> toInt32 :: Int -> Int32
> toInt32 i
>   | i == fromIntegral i32 = i32
>   | otherwise = error ("offset was too large for Int32: " ++ show i)
>   where i32 = fromIntegral i
