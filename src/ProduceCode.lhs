-----------------------------------------------------------------------------
$Id: ProduceCode.lhs,v 1.17 1999/03/12 16:07:49 simonm Exp $

The code generator.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module ProduceCode (produceParser, str, interleave, interleave') where

> import Version		( version )
> import GenUtils
> import AbsSyn
> import Grammar
> import Target			( Target(..) )

> import Maybe 			( isJust )
> import Array
> import Char (isDigit)

%-----------------------------------------------------------------------------
Produce the complete output file.

> produceParser :: Grammar 			-- grammar info
>		-> ActionTable 			-- action table
>		-> GotoTable 			-- goto table
>		-> Maybe (String,String)	-- lexer
>		-> [(Int,String)]		-- token reps
>		-> String			-- token type
>		-> String			-- parser name
>		-> (Maybe (String,String,String)) -- optional monad
>		-> Maybe String			-- module header
>		-> Maybe String			-- module trailer
>		-> Target			-- type of code required
>		-> Bool				-- use coercions
>		-> String

> produceParser (Grammar 
>		{ productions = prods
>		, lookupProdNo = lookupProd
>		, lookupProdsOfName = lookupProdNos
>		, non_terminals = nonterms
>		, terminals = terms
>		, types = nt_types
>		, token_names = names
>		, eof_term = eof
>		, first_term = fst_term
>		})
>	 	action goto lexer token_rep token_type
>		name monad module_header module_trailer target coerce 
>     =	( str comment
>	. maybestr module_header . nl
> 	. produceAbsSynDecl . nl
>    	. produceTypes
>	. produceActionTable target
>	. produceReductions
>	. produceTokenConverter . nl
>	. produceMonadStuff
>	. (if (not . null) name 
>		then (str name . str " = happyParse\n\n") 
>		else id)
>	. maybestr module_trailer
>	) ""
>   where

%-----------------------------------------------------------------------------
Make the abstract syntax type declaration, of the form:

data HappyAbsSyn a t1 .. tn
	= HappyTerminal a
	| HappyAbsSyn1 t1
	...
	| HappyAbsSynn tn

>    produceAbsSynDecl 

If we're using coercions, no need for an abstract syntax type at all:

>     | coerce = id

Otherwise, output the declaration in full...

>     | otherwise
>	= str "data HappyAbsSyn "
>	. str (unwords [ "t" ++ show n | (n, Nothing) <- assocs nt_types ])
>	. str "\n\t= HappyTerminal " . str token_type
>	. str "\n\t| HappyErrorToken Int\n"
>	. interleave "\n" 
>         [ str "\t| " . 
>             makeAbsSynCon n . 
>             (case ty of 
>                 Nothing -> showString " t" . shows n
>                 Just t  -> brack t)
>         | (n, ty) <- assocs nt_types, 
>	    (nt_types_index ! n) == n]

%-----------------------------------------------------------------------------
Type declarations of the form:

type HappyReduction a b = ....
action_0, action_1 :: Int -> HappyReduction a b 
reduction_1, ...   :: HappyReduction a b 

These are only generated if types for *all* rules are given (and not for array
based parsers -- types aren't as important there).

>    produceTypes 
>     | coerce = id		-- ToDo.

>     | target == TargetArrayBased = id

>     | all isJust (elems nt_types) =
>       str "type HappyReduction = \n\t"
>     . str "   "
>     . intMaybeHash
>     . str " \n\t-> " . token
>     . str "\n\t-> HappyState "
>     . token
>     . str " ([HappyAbsSyn] -> " . tokens . result
>     . str ")\n\t"
>     . str "-> [HappyState "
>     . token
>     . str " ([HappyAbsSyn] -> " . tokens . result
>     . str ")] \n\t-> [HappyAbsSyn] \n\t-> "
>     . tokens
>     . result
>     . str "\n\n"
>     . interleave' ",\n " 
>             [ mkActionName i | (i,action) <- zip [ 0 :: Int .. ] 
>                                             (assocs action) ]
>     . str " :: "
>     . intMaybeHash
>     . str " -> HappyReduction\n\n"
>     . interleave' ",\n " 
>             [ mkReductionFun i | 
>                     (i,action) <- zip [ 1 :: Int .. ]
>                                       (tail prods) ]
>     . str " :: HappyReduction\n\n" 

>     | otherwise = id

>	where intMaybeHash 
>		= case target of
>			TargetGhc -> str "Int#"
>			_	  -> str "Int"
>	      token = brack token_type
>	      tokens = 
>     		case lexer of
>	  		Nothing -> char '[' . token . str "] -> "
>	  		Just _ -> id
>	      result = mkMonadTy (str res_type)
> 	      (Just res_type) = nt_types ! firstNT

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

>    produceReductions =
> 	interleave "\n\n" (zipWith produceReduction (tail prods) [ 1 .. ])

>    produceReduction (nt, toks, sem) i

>     | isMonadProd
>	= mkReductionHdr (showInt lt) 
>		"happyMonadReduce " (strspace . this_absSynCon)
>	. char '(' . interleave " :\n\t" tokPatterns
>	. str "happyRest)\n\t = "
>	. tokLets
>	. typed_code
>       . defaultCase
>	. char '}'

>     | specReduceFun lt
>	= mkReductionHdr (shows lt) "happySpecReduce_" id
>	. interleave "\n\t" tokPatterns
>	. str " =  "
>	. this_absSynCon . str "\n\t\t " . brack' typed_code
>	. (if coerce || null toks || null vars_used then
>		  id
>	   else
>		  str ";\n  reduction " 
> 		. interleave " " (map str (take (length toks) (repeat "_")))
>		. str " = notHappyAtAll ")
>	. char '}'

>     | otherwise
> 	= mkReductionHdr (showInt lt) "happyReduce "  id
>	. char '(' . interleave " :\n\t" tokPatterns
>	. str "happyRest)\n\t = "
>	. tokLets
>	. this_absSynCon . str "\n\t\t " . brack' typed_code . str " : happyRest"
>	. defaultCase
>	. char '}'

>       where 
>		isMonadProd = case sem of ('%' : code) -> True
>			 		  _            -> False
> 
>		mkReductionHdr lt s arg = 
>			mkReductionFun i . str " = "
>			. str s . lt . strspace . showInt nt . arg
>			. strspace
>			. reduction
>			. str " where {\n  reduction\n\t"
> 
>		reduction | coerce = str "(unsafeCoerce# reduction)"
>			  | otherwise = str "reduction"
>
>		defaultCase = if not (null toks)
>              			then str ";\n  reduction _ = notHappyAtAll "
>              			else id
> 
>		tokPatterns 
>		 | coerce && (isMonadProd || not (specReduceFun lt)) = 
>			reverse (map mkDummyVar [1 .. length toks])
>		 | otherwise = reverse (zipWith tokPattern [1..] toks)
> 
>		tokPattern n _ | n `notElem` vars_used = char '_'
>             	tokPattern n t | t >= startTok && t < fst_term
>	      		= if coerce 
>				then mkHappyVar n
>			  	else brack' (
>				     makeAbsSynCon t . str "  " . mkHappyVar n
>				     )
>		tokPattern n t
>			= if coerce
>				then mkHappyTerminalVar n t
>				else str "(HappyTerminal " 
>				   . mkHappyTerminalVar n t
>				   . char '}'
>		
>		tokLets 
>		   | coerce && not (null toks) = 
>		          str "let {\n\t" 
>			. interleave "; \n\t"
>				[ tokPattern n t . str " = " . 
>				  str "(unsafeCoerce# " .
>				  mkDummyVar n  . char ')'
>				| (n,t) <- zip [1..] toks ]
>			. str "} in\n\t\t"
>
>		   | otherwise = id
>
>		(code,vars_used) = expandVars sem
>
>		code' 
>		    | isMonadProd = tail code  -- drop the '%'
>		    | otherwise   = code
>
>		maybe_ty = nt_types ! nt
>		has_ty = isJust maybe_ty
>		(Just ty) = maybe_ty
>
>		item_ty 
>		    | isMonadProd = mkMonadTy (str ty)
>		    | otherwise   = str ty
>
>		-- add a type signature if we're using coercions
>		typed_code
>		   | coerce = 
>			if has_ty then brack code' . str " :: " . item_ty
>				  else error ("Missing type signature for `" ++
>					      names ! nt ++ "'")
>		   | otherwise = str code'
> 
>		lt = length toks

>		this_absSynCon | coerce = if isMonadProd then 
>						str "unsafeCoerce#"
>					  else id
>			       | otherwise = makeAbsSynCon nt

%-----------------------------------------------------------------------------
The token conversion function.

>    produceTokenConverter
>	= case lexer of { 
> 
>	Nothing ->
>    	  str "happyNewToken action sts stk [] =\n\t"
>    	. eofAction
>	. str " []\n\n"
>       . str "happyNewToken action sts stk (tk:tks) =\n\t"
>	. str "let cont i = " . doAction . str " sts stk tks in\n\t"
>	. str "case tk of {\n\t"
>	. interleave ";\n\t" (map doToken token_rep)
>	. str "}\n";

>	Just (lexer,eof) ->
>	  str "happyNewToken action sts stk\n\t= "
>	. str lexer
>	. str "(\\tk -> "
>	. str "\n\tlet cont i = "
>	. doAction
>	. str " sts stk in\n\t"
>	. str "case tk of {\n\t"
>	. str (eof ++ " -> ")
>    	. eofAction . str ";\n\t"
>	. interleave ";\n\t" (map doToken token_rep)
>	. str "})\n"
>	}

>	where 

>	  eofAction = 
>	    (case target of
>	    	TargetArrayBased ->
>	   	  str "happyDoAction " . eofTok . eofError . str " action"
>	    	_ ->  str "action "	. eofTok . strspace . eofTok . eofError
>		    . str " (HappyState action)")
>	     . str " sts stk"
>	  eofError = str " (error \"reading EOF!\")"
>	  eofTok = showInt (tokIndex eof)
>	
>	  doAction = case target of
>	    TargetArrayBased -> str "happyDoAction i tk action"
>	    _   -> str "action i i tk (HappyState action)"
> 
>	  doToken (i,tok) 
>		= str (removeDollorDollor tok)
>		. str " -> cont " 
>		. showInt (tokIndex i)

Use a variable rather than '_' to replace '$$', so we can use it on
the left hand side of '@'.

>	  removeDollorDollor xs = case mapDollarDollar xs of
>				   Nothing -> xs
>				   Just fn -> fn "happy_dollar_dollar"

>    mkHappyTerminalVar :: Int -> Int -> String -> String
>    mkHappyTerminalVar i t = 
>     case tok_str_fn of
>	Nothing -> pat 
>	Just fn -> brack (fn (pat []))
>     where
>	  tok_str_fn = case lookup t token_rep of
>		      Nothing -> Nothing
>		      Just str -> mapDollarDollar str
>	  pat = mkHappyVar i

>    tokIndex 
>	= case target of
>		TargetGhc	 -> id
>		TargetHaskell 	 -> id
>		TargetArrayBased -> \i ->i - n_nonterminals - 1


%-----------------------------------------------------------------------------
Action Tables.

Here we do a bit of trickery and replace the normal default action
(failure) for each state with a reduction under the following
circumstances:

i)  there is at least one reduction action in this state.
ii) if there is more than one reduction action, they reduce using the same rule.

If these conditions hold, then the reduction becomes the default
action.  This should make the code smaller without affecting the
speed.  It changes the sematics for errors, however; errors could be
detected in a different state now.

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
   * otherwise pick the most popular reduction in this state for the default.
   * if there are no reduce actions in this state, then the default
     action remains 'enter error recovery'.

This gives us an invariant: there won't ever be a production of the
type 'error -> reduce_n' explicitly in the grammar, which means that
whenever an unexpected token occurs, either the parser will reduce
straight back to a state where the error token can be shifted, or if
none exists, we'll get a parse error.  In theory, we won't need the
machinery to discard states in the parser...

>    produceActionTable TargetHaskell 
>	= foldr (.) id (map (produceStateFunction goto) (assocs action))
>    produceActionTable TargetGhc
>	= foldr (.) id (map (produceStateFunction goto) (assocs action))
>	
>    produceActionTable TargetArrayBased
> 	= produceActionArray
>	. produceGotoArray
>	. produceReduceArray
>	. str "happy_n_terms = " . shows n_terminals . str " :: Int\n"
>	. str "happy_n_nonterms = " . shows n_nonterminals . str " :: Int\n\n"

>    produceStateFunction goto (state, acts)
> 	= foldr (.) id (map produceActions assocs_acts)
>	. foldr (.) id (map produceGotos   (assocs gotos))
>	. mkActionName state
>	. case target of
>              TargetGhc ->   str " x = happyTcHack x "
>              _         ->   str " _ = "
>	. mkAction default_act
>	. str "\n\n"
>
>	where gotos = goto ! state
>	
>	      produceActions (t, LR'Fail{-'-}) = id
>	      produceActions (t, action@(LR'Reduce{-'-} _))
>	      	 | action == default_act = id
>		 | otherwise = actionFunction t
>			     . mkAction action . str "\n"
>	      produceActions (t, action)
>	      	= actionFunction t
>		. mkAction action . str "\n"
>		
>	      produceGotos (t, Goto i)
>	        = actionFunction t
>		. str "happyGoto " . mkActionName i . str "\n"
>	      produceGotos (t, NoGoto) = id
>	      
>	      actionFunction t
>	      	= mkActionName state . strspace
>		. ('(' :) . showInt t
>		. str ") = "
>		
> 	      default_act = getDefault assocs_acts
>
>	      assocs_acts = assocs acts

action array indexed by (terminal * last_state) + state

>    produceActionArray
>   	= str "happyActionArr :: Array Int Int\n"
>	. str "happyActionArr = listArray (-1,"
>		. shows (n_terminals * n_states)
>		. str ") (["
>	. rle_to_str (rle_list (concat 
>		(map actionArrElems (elems action)))) False
>	. str "\n\t])\n\n"
>	
>    (_, last_state) = bounds action
>    n_states = last_state + 1
>    n_terminals = length terms
>    n_nonterminals = length nonterms - 1 -- lose one for %start
>
>    actionArrElems actions = map actionVal (e : drop (n_nonterminals + 1) line)
>	where (e:line) = elems actions

>    produceGotoArray
>   	= str "happyGotoArr :: Array Int Int\n"
>	. str "happyGotoArr = listArray (0, "
>		. shows (n_nonterminals * n_states)
>		. str ") (["
>	. rle_to_str (rle_list (concat 
>		(map gotoArrElems (elems goto)))) False
>	. str "\n\t])\n\n"

>    gotoArrElems gotos	= map gotoVal (elems gotos)

>    produceReduceArray
>   	= {- str "happyReduceArr :: Array Int a\n" -}
>	  str "happyReduceArr = array ("
>		. shows (1 :: Int)
>		. str ", "
>		. shows n_rules
>		. str ") [\n"
>	. interleave' ",\n" (map reduceArrElem [1..n_rules])
>	. str "\n\t]\n\n"

>    n_rules = length prods - 1 :: Int

>    showInt i = case target of
>    		TargetGhc -> shows i . showChar '#'
>		_	  -> shows i

This lets examples like:

	data HappyAbsSyn t1
		= HappyTerminal ( HaskToken )
		| HappyAbsSyn1 (  HaskExp  )
		| HappyAbsSyn2 (  HaskExp  )
		| HappyAbsSyn3 t1

*share* the defintion for ( HaskExp )

	data HappyAbsSyn t1
		= HappyTerminal ( HaskToken )
		| HappyAbsSyn1 (  HaskExp  )
		| HappyAbsSyn3 t1

... cuting down on the work that the type checker has to do.

Note, this *could* introduce lack of polymophism,
for types that have alphas in them. Maybe we should
outlaw them inside { }

>    nt_types_index = array (bounds nt_types) 
>			[ (a, fn a b) | (a, b) <- assocs nt_types ]
>     where
>	fn n Nothing = n
>	fn n (Just a) = case lookup a assoc_list of
>			  Just v -> v
>			  Nothing -> error ("cant find an item in list")
>	assoc_list = [ (b,a) | (a, Just b) <- assocs nt_types ]

>    makeAbsSynCon = mkAbsSynCon nt_types_index

>    mkMonadTy s = case monad of
>			Nothing -> s
>			Just (ty,_,_) -> str (ty++"(") . s . char ')'

>    produceMonadStuff =
>	(case monad of
>	  Nothing -> 
>            str "happyThen = \\m k -> k m\n" .
>	     str "happyReturn = " .
>            (case lexer of 
>		  Nothing -> str "\\a tks -> a"
>		  _       -> str "\\a -> a")
>	  Just (ty,tn,rtn) ->
>            let pty = str ty in
>            str "happyThen :: " . pty .
>            str " a -> (a -> "  . pty . 
>	     str " b) -> " . pty . str " b\n" .
>            str "happyThen = (" . str tn . str ")\n" .
>            str "happyReturn = " . 
>	     (case lexer of
>		 Nothing -> str "\\tks ->"
>		 _       -> id)
>		. str rtn)
>	. str "\n"

>    reduceArrElem n
>      = str "\t(" . shows n . str " , "
>      . str "happyReduce_" . shows n . char ')'

-----------------------------------------------------------------------------
Replace all the $n variables with happy_vars, and return a list of all the
vars used in this piece of code.

>    expandVars :: String -> (String,[Int])
>    expandVars [] = ("",[])
>    expandVars ('$':r) 
>    	   | isDigit (head r) = ("happy_var_" ++ num ++ code, read num : vars)
>    	   | otherwise = error ("Illegal attribute: $" ++ [head r] ++ "\n")
>    	where
>    	   (num,rest)  = span isDigit r
>    	   (code,vars) = expandVars rest
>    expandVars (c:r) = (c:code,vars)
>    	where
>	(code,vars) = expandVars r

> actionVal :: LRAction -> Int
> actionVal (LR'Shift  state) 	= state + 1
> actionVal (LR'Reduce rule)  	= -(rule + 1)
> actionVal  LR'Accept		= -1
> actionVal  LR'Fail		= 0
> actionVal (LR'Multiple _ a)	= actionVal a

> gotoVal :: Goto -> Int
> gotoVal (Goto i)		= i
> gotoVal NoGoto		= 0
  
> mkAction (LR'Shift i)	 	= str "happyShift " . mkActionName i
> mkAction LR'Accept 	 	= str "happyAccept"
> mkAction LR'Fail 	 	= str "happyFail"
> mkAction (LR'Reduce i) 	= str "happyReduce_" . shows i
> mkAction (LR'Multiple as a)	= mkAction a

> mkActionName i		= str "action_" . shows i

> getDefault actions =
>   case [ act | (-1,act@(LR'Reduce{-'-} _)) <- actions ] of
>	(act:_) -> act	-- use error reduction if there is one.
>	[] ->
>	    case reduces of
>		 [] -> LR'Fail
>		 (act:_) -> act	-- pick the first one we see for now
>
>   where reduces = [ act | (_,act@(LR'Reduce{-'-} _)) <- actions ]
>   		    ++ [ act | (_,(LR'Multiple{-'-} _ 
>					act@(LR'Reduce{-'-} _))) <- actions ]

-----------------------------------------------------------------------------
Misc.

> comment = 
> 	"-- parser produced by Happy Version " ++ version ++ "\n\n"

> str = showString
> char c = (c :)
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' s = foldr1 (\a b -> a . str s . b) 

> strspace = char ' '
> nl = char '\n'

> mkAbsSynCon fx t    	= str "HappyAbsSyn"   . shows (fx ! t)
> mkHappyVar n     	= str "happy_var_"    . shows n
> mkReductionFun n 	= str "happyReduce_"  . shows n
> mkDummyVar n		= str "happy_x_"      . shows n

> specReduceFun 	= (<= (3 :: Int))

> maybestr (Just s)	= str s
> maybestr _		= id

> mapDollarDollar :: String -> Maybe (String -> String)
> mapDollarDollar "" = Nothing
> mapDollarDollar ('$':'$':r) = -- only map first instance
>    case mapDollarDollar r of
>	Just fn -> error "more that one $$ in pattern"
>	Nothing -> Just (\ s -> s ++ r)
> mapDollarDollar (c:r) =
>    case mapDollarDollar r of
>	Just fn -> Just (\ s -> c : fn s)
>	Nothing -> Nothing

> brack s = str ('(' : s) . char ')'
> brack' s = char '(' . s . char ')'

-----------------------------------------------------------------------------
Run Length Encode an array to cut down on code size.

> rle_list :: [Int] -> [(Int,Int)]
> rle_list [] = []
> rle_list (x:xs) = (l_x's + 1, x) : rle_list xs'
>  where
>	(x's,xs') = span (==x) xs
>	l_x's = length x's

> rle_to_str :: [(Int,Int)] -> Bool -> ShowS
> rle_to_str [] _ = id
> rle_to_str ((l,x):xs) comma = 
>	if l > 20 then
>		  str "] ++ take " . shows l . str " (repeat " . shows x
>		. str ") ++ ["
>		. rle_to_str xs False
> 	 else
>		(if comma then char ',' else id)
>		. interleave' "," (map shows (take l (repeat x)))
>		. rle_to_str xs True
