-----------------------------------------------------------------------------
The code generator.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Backend.LALR.ProduceCode (produceParser) where

> import Paths_happy_backend_lalr  ( version )
> import Data.Version              ( showVersion )
> import Happy.CodeGen.Common.Options
> import Happy.Grammar
> import Happy.Tabular.LALR

> import Data.Maybe                ( isNothing, fromMaybe )
> import Data.Char                 ( ord, chr )
> import Data.List                 ( sortBy )

> import Control.Monad             ( forM_ )
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

> produceParser :: Grammar                      -- grammar info
>               -> CommonOptions                -- common codegen options
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
>               , attributetype = attributetype'
>               , attributes = attributes'
>               })
>               (CommonOptions
>               { lexer = lexer'
>               , imported_identity = imported_identity'
>               , monad = (use_monad,monad_context,monad_tycon,monad_then,monad_return)
>               , token_type = token_type'
>               , error_handler = error_handler'
>               , error_sig = error_sig'
>               })
>               action goto lang_exts module_header module_trailer
>               coerce strict
>     = ( top_opts
>       . maybestr module_header . nl
>       . str comment
>               -- comment goes *after* the module header, so that we
>               -- don't screw up any OPTIONS pragmas in the header.
>       . produceAbsSynDecl . nl
>       . produceExpListPerState
>       . produceActionTable
>       . produceReductions
>       . produceTokenConverter . nl
>       . produceIdentityStuff
>       . produceMonadStuff
>       . produceEntries
>       . produceStrict strict
>       . produceAttributes attributes' attributetype' . nl
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
>
>    partTySigs_opts = ifGeGhc710 (str "{-# LANGUAGE PartialTypeSignatures #-}" . nl)

>    intMaybeHash    = str "Happy_GHC_Exts.Int#"

>    -- Parsing monad and its constraints
>    pty = str monad_tycon
>    pcont = str monad_context
>
>    -- If GHC is enabled, wrap the content in a CPP ifdef that includes the
>    -- content and tests whether the GHC version is >= 7.10.3
>    ifGeGhc710 :: (String -> String) -> String -> String
>    ifGeGhc710 content  = str "#if __GLASGOW_HASKELL__ >= 710" . nl
>                        . content
>                        . str "#endif" . nl

>    n_missing_types = length (filter isNothing (elems nt_types))
>    happyAbsSyn = str "(HappyAbsSyn " . str wild_tyvars . str ")"
>      where wild_tyvars = unwords (replicate n_missing_types "_")
>
>    -- This decides how to include (if at all) a type signature
>    -- See <https://github.com/haskell/happy/issues/94>
>    filterTypeSig :: (String -> String) -> String -> String
>    filterTypeSig content | n_missing_types == 0 = content
>                          | otherwise = ifGeGhc710 content
>
>    top_opts =
>        nowarn_opts
>      . (str $ unlines
>           [ unwords [ "{-# LANGUAGE", l, "#-}" ] | l <- lang_exts ])
>      . partTySigs_opts

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
>       . str "\n\t= HappyTerminal " . token
>       . str "\n\t| HappyErrorToken Prelude.Int\n"
>       . interleave "\n"
>         [ str "\t| " . makeAbsSynCon n . strspace . typeParam n ty
>         | (n, ty) <- assocs nt_types,
>           (nt_types_index ! n) == n]

>     where all_tyvars = [ 't':show n | (n, Nothing) <- assocs nt_types ]
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
>       . char '(' . interleave " `HappyStk`\n\t" tokPatterns
>       . str "happyRest) tk\n\t = happyThen ("
>       . str "("
>       . tokLets (char '(' . str code' . char ')')
>       . str ")"
>       . (if monad_pass_token then str " tk" else id)
>       . str "\n\t) (\\r -> happyReturn (" . this_absSynCon . str " r))"

>     | specReduceFun lt
>       = mkReductionHdr id ("happySpecReduce_" ++ show lt)
>       . interleave "\n\t" tokPatterns
>       . str " =  "
>       . tokLets (
>           this_absSynCon . str "\n\t\t "
>           . char '(' . str code' . str "\n\t)"
>         )
>       . (if coerce || null toks || null vars_used then
>                 id
>          else
>                 nl . reductionFun . strspace
>               . interleave " " (replicate (length toks) (str "_"))
>               . str " = notHappyAtAll ")

>     | otherwise
>       = mkReductionHdr (showInt lt) "happyReduce"
>       . char '(' . interleave " `HappyStk`\n\t" tokPatterns
>       . str "happyRest)\n\t = "
>       . tokLets
>          ( this_absSynCon . str "\n\t\t "
>          . char '(' . str code'. str "\n\t) `HappyStk` happyRest"
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
>               adjusted_nt = nt - first_nonterm'

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
>                       in filterTypeSig tysig . mkReduceFun i . str " = "
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
>                       = interleave "\n\t" cases
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
>       = case lexer' of {
>
>       Nothing ->
>         str "happyNewToken action sts stk [] =\n\t"
>       . eofAction "notHappyAtAll"
>       . str " []\n\n"
>       . str "happyNewToken action sts stk (tk:tks) =\n\t"
>       . str "let cont i = " . doAction . str " sts stk tks in\n\t"
>       . str "case tk of {\n\t"
>       . interleave ";\n\t" (map doToken token_rep)
>       . str "_ -> happyError' ((tk:tks), [])\n\t"
>       . str "}\n\n"
>       . str "happyError_ explist " . eofTok . str " tk tks = happyError' (tks, explist)\n"
>       . str "happyError_ explist _ tk tks = happyError' ((tk:tks), explist)\n";
>             -- when the token is EOF, tk == _|_ (notHappyAtAll)
>             -- so we must not pass it to happyError'

>       Just (lexer'',eof') ->
>         str "happyNewToken action sts stk\n\t= "
>       . str lexer''
>       . str "(\\tk -> "
>       . str "\n\tlet cont i = "
>       . doAction
>       . str " sts stk in\n\t"
>       . str "case tk of {\n\t"
>       . str (eof' ++ " -> ")
>       . eofAction "tk" . str ";\n\t"
>       . interleave ";\n\t" (map doToken token_rep)
>       . str "_ -> happyError' (tk, [])\n\t"
>       . str "})\n\n"
>       . str "happyError_ explist " . eofTok . str " tk = happyError' (tk, explist)\n"
>       . str "happyError_ explist _ tk = happyError' (tk, explist)\n";
>             -- superfluous pattern match needed to force happyError_ to
>             -- have the correct type.
>       }

>       where

>         eofAction tk = str "happyDoAction "
>                      . eofTok . strspace . str tk
>                      . str " action" . str " sts stk"
>         eofTok = showInt (tokIndex eof)

>         doAction = str "happyDoAction i tk action"

>         doToken (i,tok)
>               = str (removeDollarDollar tok)
>               . str " -> cont "
>               . showInt (tokIndex i)

Use a variable rather than '_' to replace '$$', so we can use it on
the left hand side of '@'.

>         removeDollarDollar xs = case mapDollarDollar xs of
>                                  Nothing -> xs
>                                  Just fn -> fn "happy_dollar_dollar"

>    mkHappyTerminalVar :: Int -> Int -> String -> String
>    mkHappyTerminalVar i t =
>     case tok_str_fn of
>       Nothing -> pat
>       Just fn -> brack (fn (pat []))
>     where
>         tok_str_fn = case lookup t token_rep of
>                     Nothing -> Nothing
>                     Just str' -> mapDollarDollar str'
>         pat = mkHappyVar i

>    tokIndex i = i - n_nonterminals - n_starts - 2
>                       -- tokens adjusted to start at zero, see ARRAY_NOTES

%-----------------------------------------------------------------------------
Action Tables.

Here we do a bit of trickery and replace the normal default action
(failure) for each state with at least one reduction action.  For each
such state, we pick one reduction action to be the default action.
This should make the code smaller without affecting the speed.  It
changes the sematics for errors, however; errors could be detected in
a different state now (but they'll still be detected at the same point
in the token stream).

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
>       . str "happy_n_terms = " . shows n_terminals . str " :: Prelude.Int\n"
>       . str "happy_n_nonterms = " . shows n_nonterminals . str " :: Prelude.Int\n\n"

>    produceExpListPerState
>       = produceExpListArray
>       . str "{-# NOINLINE happyExpListPerState #-}\n"
>       . str "happyExpListPerState st =\n"
>       . str "    token_strs_expected\n"
>       . str "  where token_strs = " . str (show $ elems token_names') . str "\n"
>       . str "        bit_start = st               Prelude.* " . str (show nr_tokens) . str "\n"
>       . str "        bit_end   = (st Prelude.+ 1) Prelude.* " . str (show nr_tokens) . str "\n"
>       . str "        read_bit = readArrayBit happyExpList\n"
>       . str "        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]\n"
>       . str "        bits_indexed = Prelude.zip bits [0.."
>                                        . str (show (nr_tokens - 1)) . str "]\n"
>       . str "        token_strs_expected = Prelude.concatMap f bits_indexed\n"
>       . str "        f (Prelude.False, _) = []\n"
>       . str "        f (Prelude.True, nr) = [token_strs Prelude.!! nr]\n"
>       . str "\n"
>       where (first_token, last_token) = bounds token_names'
>             nr_tokens = last_token - first_token + 1

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
>           . str "happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#\n"
>           . str "happyAdjustOffset off = "
>           . (if length table < 32768
>                then str "off"
>                else str "if happyLt off (" . shows min_off . str "# :: Happy_GHC_Exts.Int#)"
>                   . str " then off Happy_GHC_Exts.+# 65536#"
>                   . str " else off")
>           . str "\n\n"  --"
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


>    produceExpListArray
>           = str "happyExpList :: HappyAddr\n"
>           . str "happyExpList = HappyA# \"" --"
>           . hexCharsForBits explist
>           . str "\"#\n\n" --"

>    n_terminals = length terms
>    n_nonterminals = length nonterms - n_starts -- lose %starts
>
>    (act_offs,goto_offs,table,defaults,check,explist,min_off)
>       = mkTables action goto first_nonterm' fst_term
>               n_terminals n_nonterminals n_starts (bounds token_names')
>
>    produceReduceArray
>       = {- str "happyReduceArr :: Array Int a\n" -}
>         str "happyReduceArr = Happy_Data_Array.array ("
>               . shows (n_starts :: Int) -- omit the %start reductions
>               . str ", "
>               . shows n_rules
>               . str ") [\n"
>       . interleave' ",\n" (map reduceArrElem [n_starts..n_rules])
>       . str "\n\t]\n\n"

>    n_rules = length prods - 1 :: Int

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

>    nt_types_index :: Array Int Int
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
>          . str "instance Prelude.Functor HappyIdentity where\n"
>          . str "    fmap f (HappyIdentity a) = HappyIdentity (f a)\n\n"
>          . str "instance Applicative HappyIdentity where\n"
>          . str "    pure  = HappyIdentity\n"
>          . str "    (<*>) = ap\n"
>          . str "instance Prelude.Monad HappyIdentity where\n"
>          . str "    return = pure\n"
>          . str "    (HappyIdentity p) >>= q = q p\n\n"

MonadStuff:

  - with no %monad or %lexer:

        happyThen    :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
        happyReturn  :: () => a -> HappyIdentity a
        happyThen1   m k tks = happyThen m (\a -> k a tks)
        happyReturn1 = \a tks -> happyReturn a

  - with %monad:

        happyThen    :: CONTEXT => P a -> (a -> P b) -> P b
        happyReturn  :: CONTEXT => a -> P a
        happyThen1   m k tks = happyThen m (\a -> k a tks)
        happyReturn1 = \a tks -> happyReturn a

  - with %monad & %lexer:

        happyThen    :: CONTEXT => P a -> (a -> P b) -> P b
        happyReturn  :: CONTEXT => a -> P a
        happyThen1   = happyThen
        happyReturn1 = happyReturn


>    produceMonadStuff =
>            str "happyThen :: " . pcont . str " => " . pty
>          . str " a -> (a -> "  . pty
>          . str " b) -> " . pty . str " b\n"
>          . str "happyThen = " . brack monad_then . nl
>          . str "happyReturn :: " . pcont . str " => a -> " . pty . str " a\n"
>          . str "happyReturn = " . brack monad_return . nl
>          . case lexer' of
>               Nothing ->
>                  str "happyThen1 m k tks = (" . str monad_then
>                . str ") m (\\a -> k a tks)\n"
>                . str "happyReturn1 :: " . pcont . str " => a -> b -> " . pty . str " a\n"
>                . str "happyReturn1 = \\a tks -> " . brack monad_return
>                . str " a\n"
>                . str "happyError' :: " . str monad_context . str " => (["
>                . token
>                . str "], [Prelude.String]) -> "
>                . str monad_tycon
>                . str " a\n"
>                . str "happyError' = "
>                . str (if use_monad then "" else "HappyIdentity Prelude.. ")
>                . errorHandler . str "\n"
>               _ ->
>                let
>                  happyParseSig =
>                        str "happyParse :: " . pcont . str " => " . intMaybeHash
>                      . str " -> " . pty . str " " . happyAbsSyn . str "\n"
>                      . str "\n"
>                  newTokenSig =
>                        str "happyNewToken :: " . pcont . str " => " . intMaybeHash
>                      . str " -> Happy_IntList -> HappyStk " . happyAbsSyn
>                      . str " -> " . pty . str " " . happyAbsSyn . str"\n"
>                      . str "\n"
>                  doActionSig =
>                        str "happyDoAction :: " . pcont . str " => " . intMaybeHash
>                      . str " -> " . str token_type' . str " -> " . intMaybeHash
>                      . str " -> Happy_IntList -> HappyStk " . happyAbsSyn
>                      . str " -> " . pty . str " " . happyAbsSyn . str "\n"
>                      . str "\n"
>                  reduceArrSig =
>                        str "happyReduceArr :: " . pcont
>                      . str " => Happy_Data_Array.Array Prelude.Int (" . intMaybeHash
>                      . str " -> " . str token_type' . str " -> " . intMaybeHash
>                      . str " -> Happy_IntList -> HappyStk " . happyAbsSyn
>                      . str " -> " . pty . str " " . happyAbsSyn . str ")\n"
>                      . str "\n"
>                  in filterTypeSig (happyParseSig . newTokenSig . doActionSig . reduceArrSig)
>                . str "happyThen1 :: " . pcont . str " => " . pty
>                . str " a -> (a -> "  . pty
>                . str " b) -> " . pty . str " b\n"
>                . str "happyThen1 = happyThen\n"
>                . str "happyReturn1 :: " . pcont . str " => a -> " . pty . str " a\n"
>                . str "happyReturn1 = happyReturn\n"
>                . str "happyError' :: " . str monad_context . str " => ("
>                                        . token . str ", [Prelude.String]) -> "
>                . str monad_tycon
>                . str " a\n"
>                . str "happyError' tk = "
>                . str (if use_monad then "" else "HappyIdentity ")
>                . errorHandler . str " tk\n"

An error handler specified with %error is passed the current token
when used with %lexer, but happyError (the old way but kept for
compatibility) is not passed the current token. Also, the %errorhandlertype
directive determines the API of the provided function.

>    errorHandler =
>       case error_handler' of
>               Just h  -> case error_sig' of
>                              ErrorHandlerTypeExpList -> str h
>                              ErrorHandlerTypeDefault -> str "(\\(tokens, _) -> " . str h . str " tokens)"
>               Nothing -> case lexer' of
>                               Nothing -> str "(\\(tokens, _) -> happyError tokens)"
>                               Just _  -> str "(\\(tokens, explist) -> happyError)"

>    reduceArrElem n
>      = str "\t(" . shows n . str " , "
>      . str "happyReduce_" . shows n . char ')'

-----------------------------------------------------------------------------
-- Produce the parser entry and exit points

>    produceEntries
>       = interleave "\n\n" (map produceEntry (zip starts' [0..]))
>       . if null attributes' then id else produceAttrEntries starts'

>    produceEntry :: ((String, t0, Int, t1), Int) -> String -> String
>    produceEntry ((name, _start_nonterm, accept_nonterm, _partial), no)
>       = (if null attributes' then str name else str "do_" . str name)
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
>                       . shows (nt_types_index ! accept_nonterm)
>                       . str " z -> happyReturn z; _other -> notHappyAtAll }"
>                )
>     where
>       maybe_tks | isNothing lexer' = str " tks"
>                 | otherwise = id
>       unmonad | use_monad = ""
>                 | otherwise = "happyRunIdentity "

>    produceAttrEntries starts''
>       = interleave "\n\n" (map f starts'')
>     where
>       f = case (use_monad,lexer') of
>             (True,Just _)  -> \(name,_,_,_) -> monadAndLexerAE name
>             (True,Nothing) -> \(name,_,_,_) -> monadAE name
>             (False,Just _) -> error "attribute grammars not supported for non-monadic parsers with %lexer"
>             (False,Nothing)-> \(name,_,_,_) -> regularAE name
>
>       defaultAttr = fst (head attributes')
>
>       monadAndLexerAE name
>         = str name . str " = "
>         . str "do { "
>         . str "f <- do_" . str name . str "; "
>         . str "let { (conds,attrs) = f happyEmptyAttrs } in do { "
>         . str "Prelude.sequence_ conds; "
>         . str "Prelude.return (". str defaultAttr . str " attrs) }}"
>       monadAE name
>         = str name . str " toks = "
>         . str "do { "
>         . str "f <- do_" . str name . str " toks; "
>         . str "let { (conds,attrs) = f happyEmptyAttrs } in do { "
>         . str "Prelude.sequence_ conds; "
>         . str "Prelude.return (". str defaultAttr . str " attrs) }}"
>       regularAE name
>         = str name . str " toks = "
>         . str "let { "
>         . str "f = do_" . str name . str " toks; "
>         . str "(conds,attrs) = f happyEmptyAttrs; "
>         . str "x = Prelude.foldr Prelude.seq attrs conds; "
>         . str "} in (". str defaultAttr . str " x)"

----------------------------------------------------------------------------
-- Produce attributes declaration for attribute grammars

> produceAttributes :: [(String, String)] -> String -> String -> String
> produceAttributes [] _ = id
> produceAttributes attrs attributeType
>     = str "data " . attrHeader . str " = HappyAttributes {" . attributes' . str "}" . nl
>     . str "happyEmptyAttrs = HappyAttributes {" . attrsErrors . str "}" . nl

>   where attributes'  = foldl1 (\x y -> x . str ", " . y) $ map formatAttribute attrs
>         formatAttribute (ident,typ) = str ident . str " :: " . str typ
>         attrsErrors = foldl1 (\x y -> x . str ", " . y) $ map attrError attrs
>         attrError (ident,_) = str ident . str " = Prelude.error \"invalid reference to attribute '" . str ident . str "'\""
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
> getDefault actions =
>   -- pick out the action for the error token, if any
>   case [ act | (e, act) <- actions, e == errorTok ] of
>
>       -- use error reduction as the default action, if there is one.
>       act@(LR'Reduce _ _) : _                 -> act
>       act@(LR'Multiple _ (LR'Reduce _ _)) : _ -> act
>
>       -- if the error token is shifted or otherwise, don't generate
>       --  a default action.  This is *important*!
>       (act : _) | act /= LR'Fail -> LR'Fail
>
>       -- no error actions, pick a reduce to be the default.
>       _      -> case reduces of
>                     [] -> LR'Fail
>                     (act:_) -> act    -- pick the first one we see for now
>
>   where reduces
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


> mkTables
>        :: ActionTable -> GotoTable -> Name -> Int -> Int -> Int -> Int -> (Int, Int) ->
>        ( [Int]         -- happyActOffsets
>        , [Int]         -- happyGotoOffsets
>        , [Int]         -- happyTable
>        , [Int]         -- happyDefAction
>        , [Int]         -- happyCheck
>        , [Int]         -- happyExpList
>        , Int           -- happyMinOffset
>        )
>
> mkTables action goto first_nonterm' fst_term
>               n_terminals n_nonterminals n_starts
>               token_names_bound
>
>  = ( elems act_offs
>    , elems goto_offs
>    , take max_off (elems table)
>    , def_actions
>    , take max_off (elems check)
>    , elems explist
>    , min_off
>    )
>  where
>
>        (table,check,act_offs,goto_offs,explist,min_off,max_off)
>                = runST (genTables (length actions)
>                         max_token token_names_bound
>                         sorted_actions explist_actions)
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
>                  let (err:_dummy:vec) = assocs acts
>                      vec' = drop (n_starts+n_nonterminals) vec
>                      acts' = filter notFail (err:vec')
>                      default_act = getDefault acts'
>                      acts'' = mkActVals acts' default_act
>                ]
>
>        explist_actions :: [(Int, [Int])]
>        explist_actions = [ (state, concatMap f $ assocs acts)
>                          | (state, acts) <- assocs action ]
>                          where
>                            f (t, LR'Shift _ _ ) = [t - fst token_names_bound]
>                            f (_, _) = []
>
>        -- adjust terminals by -(fst_term+1), so they start at 1 (error is 0).
>        --  (see ARRAY_NOTES)
>        adjust token | token == errorTok = 0
>                     | otherwise         = token - fst_term + 1
>
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
>                [ (token - first_nonterm', i) | (token, Goto i) <- assocs' ]
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
>        -> (Int, Int)                  -- token names bounds
>        -> [TableEntry]                -- entries for the table
>        -> [(Int, [Int])]              -- expected tokens lists
>        -> ST s ( UArray Int Int       -- table
>                , UArray Int Int       -- check
>                , UArray Int Int       -- action offsets
>                , UArray Int Int       -- goto offsets
>                , UArray Int Int       -- expected tokens list
>                , Int                  -- lowest offset in table
>                , Int                  -- highest offset in table
>                )
>
> genTables n_actions max_token token_names_bound entries explist = do
>
>   table      <- newArray (0, mAX_TABLE_SIZE) 0
>   check      <- newArray (0, mAX_TABLE_SIZE) (-1)
>   act_offs   <- newArray (0, n_actions) 0
>   goto_offs  <- newArray (0, n_actions) 0
>   off_arr    <- newArray (-max_token, mAX_TABLE_SIZE) 0
>   exp_array  <- newArray (0, (n_actions * n_token_names + 31) `div` 32) 0 -- 32 bits per entry
>
>   (min_off,max_off) <- genTables' table check act_offs goto_offs off_arr exp_array entries
>                          explist max_token n_token_names
>
>   table'     <- freeze table
>   check'     <- freeze check
>   act_offs'  <- freeze act_offs
>   goto_offs' <- freeze goto_offs
>   exp_array' <- freeze exp_array
>   return (table',check',act_offs',goto_offs',exp_array',min_off,max_off+1)

>   where
>        n_states = n_actions - 1
>        mAX_TABLE_SIZE = n_states * (max_token + 1)
>        (first_token, last') = token_names_bound
>        n_token_names = last' - first_token + 1


> genTables'
>        :: STUArray s Int Int          -- table
>        -> STUArray s Int Int          -- check
>        -> STUArray s Int Int          -- action offsets
>        -> STUArray s Int Int          -- goto offsets
>        -> STUArray s Int Int          -- offset array
>        -> STUArray s Int Int          -- expected token list
>        -> [TableEntry]                -- entries for the table
>        -> [(Int, [Int])]              -- expected tokens lists
>        -> Int                         -- maximum token no.
>        -> Int                         -- number of token names
>        -> ST s (Int,Int)              -- lowest and highest offsets in table
>
> genTables' table check act_offs goto_offs off_arr exp_array entries
>            explist max_token n_token_names
>       = fill_exp_array >> fit_all entries 0 0 1
>   where
>
>        fit_all [] min_off max_off _ = return (min_off, max_off)
>        fit_all (s:ss) min_off max_off fst_zero = do
>          (off, new_min_off, new_max_off, new_fst_zero) <- fit s min_off max_off fst_zero
>          ss' <- same_states s ss off
>          writeArray off_arr off 1
>          fit_all ss' new_min_off new_max_off new_fst_zero
>
>        fill_exp_array =
>          forM_ explist $ \(state, tokens) ->
>            forM_ tokens $ \token -> do
>              let bit_nr = state * n_token_names + token
>              let word_nr = bit_nr `div` 32
>              let word_offset = bit_nr `mod` 32
>              x <- readArray exp_array word_nr
>              writeArray exp_array word_nr (setBit x word_offset)
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
>        fit (_,_,_,_,_,[]) min_off max_off fst_zero = return (0,min_off,max_off,fst_zero)
>
>        fit (act_or_goto, state_no, _deflt, _, _, state@((t,_):_))
>           min_off max_off fst_zero = do
>                -- start at offset 1 in the table: all the empty states
>                -- (states with just a default reduction) are mapped to
>                -- offset zero.
>          off <- findFreeOffset (-t+fst_zero) check off_arr state
>          let new_min_off | furthest_left  < min_off = furthest_left
>                          | otherwise                = min_off
>              new_max_off | furthest_right > max_off = furthest_right
>                          | otherwise                = max_off
>              furthest_left  = off
>              furthest_right = off + max_token
>
>          -- trace ("fit: state " ++ show state_no ++ ", off " ++ show off ++ ", elems " ++ show state) $ do
>
>          writeArray (which_off act_or_goto) state_no off
>          addState off table check state
>          new_fst_zero <- findFstFreeSlot check fst_zero
>          return (off, new_min_off, new_max_off, new_fst_zero)

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

> notFail :: (Int, LRAction) -> Bool
> notFail (_, LR'Fail) = False
> notFail _           = True

> findFstFreeSlot :: STUArray s Int Int -> Int -> ST s Int
> findFstFreeSlot table n = do
>        i <- readArray table n
>        if i == -1 then return n
>                   else findFstFreeSlot table (n+1)

-----------------------------------------------------------------------------
-- Misc.

> comment :: String
> comment =
>         "-- parser produced by Happy Version " ++ showVersion version ++ "\n\n"

> mkAbsSynCon :: Array Int Int -> Int -> String -> String
> mkAbsSynCon fx t      = str "HappyAbsSyn"   . shows (fx ! t)

> mkHappyVar, mkReduceFun, mkDummyVar :: Int -> String -> String
> mkHappyVar n          = str "happy_var_"    . shows n
> mkReduceFun n         = str "happyReduce_"  . shows n
> mkDummyVar n          = str "happy_x_"      . shows n

> mkHappyWrap :: Int -> String -> String
> mkHappyWrap n = str "HappyWrap" . shows n

> mkHappyWrapCon :: Maybe a -> Int -> (String -> String) -> String -> String
> mkHappyWrapCon Nothing  _ s = s
> mkHappyWrapCon (Just _) n s = brack' (mkHappyWrap n . strspace . s)

> mkHappyIn, mkHappyOut :: Int -> String -> String
> mkHappyIn n           = str "happyIn"  . shows n
> mkHappyOut n          = str "happyOut" . shows n

> typeParam, typeParamOut :: Int -> Maybe String -> ShowS
> typeParam n Nothing   = char 't' . shows n
> typeParam _ (Just ty) = brack ty
> typeParamOut n Nothing  = char 't' . shows n
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

The following function is used for generating happyExpList, which is an array of
bits encoded as [Int] for legacy reasons; we don't want to check for overflow
here.

> hexCharsForBits :: [Int] -> String -> String
> hexCharsForBits is s = foldr (hexChar . fromIntegral) s is

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
