-----------------------------------------------------------------------------
$Id: ProduceCode.lhs,v 1.6 1997/09/09 16:31:50 simonm Exp $

The code generator.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module ProduceCode (produceParser, str, interleave, interleave') where

> import Version		( version )
> import GenUtils
> import AbsSyn
> import Grammar
> import Target			( Target(..) )

#if __HASKELL1__ >= 3 && ( !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ >= 200 )

> import Maybe 			( isJust )
> import Array
> import Char (isDigit)

#define ASSOC(a,b) (a , b)
#else
#define ASSOC(a,b) (a := b)
#endif

%-----------------------------------------------------------------------------
Produce the complete output file.

> produceParser :: GrammarInfo 			-- grammar info
>		-> ActionTable 			-- action table
>		-> GotoTable 			-- goto table
>		-> Maybe (String,String)	-- lexer
>		-> [(Int,String)]		-- token reps
>		-> String			-- token type
>		-> Array Int (Maybe String)     -- types of NonTerminals
>		-> String			-- parser name
>		-> (Maybe (String,String,String)) -- optional monad
>		-> Maybe String			-- module header
>		-> Maybe String			-- module trailer
>		-> Target			-- type of code required
>		-> Bool				-- support pre-1.3 haskell
>		-> String

> produceParser
> 	(GrammarInfo prods lookupProd lookupProdNos nonterms terms eof 
>		first_term) 
>	 action goto lexer token_rep token_type nt_types
> 	 name monad module_header module_trailer target opt1_2 = (
>	 
> 	  str comment
>	. maybestr module_header . str "\n"
> 	. produceAbsSynDecl . str "\n"
>    	. produceTypes
>	. produceActionTable target
>	. produceReductions
>	. produceTokenConverter . str "\n"
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
>	= str "data HappyAbsSyn "
>	. str (unwords [ "t" ++ show n | ASSOC(n, Nothing) <- assocs nt_types ])
>	. str "\n\t= HappyTerminal " . str token_type
>	. str "\n\t| HappyErrorToken Int\n"
>	. interleave "\n" 
>         [ str "\t| " . 
>             makeAbsSynCon n . 
>             (case ty of 
>                 Nothing -> showString " t" . shows n
>                 Just t  -> brack t)
>         | ASSOC(n, ty) <- assocs nt_types, 
>	    (nt_types_index ! n) == n]

%-----------------------------------------------------------------------------
Type declarations of the form:

type HappyReduction a b = ....
action_0, action_1 :: Int -> HappyReduction a b 
reduction_1, ...   :: HappyReduction a b 

These are only generated if types for *all* rules are given (and not for array
based parsers -- types aren't as important there).

>    produceTypes | target == TargetArrayBased = id

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
>	  		Nothing -> str "[" . token . str "] -> "
>	  		Just _ -> id
>	      result = mkMonadTy (str res_type)
> 	      (Just res_type) = nt_types ! 1

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

>     | isMonadProd sem
>	= mkReductionHdr (showInt lt) 
>		"happyMonadReduce " (strspace . makeAbsSynCon nt)
>	. str "(" . interleave " :\n\t" tokPatterns
>	. str "happyRest)\n\t = "
>	. str (tail code)
>       . defaultCase
>	. str "}"

>     | specReduceFun lt
>	= mkReductionHdr (shows lt) "happySpecReduce_" id
>	. interleave "\n\t" tokPatterns
>	. str " =  "
>	. makeAbsSynCon nt . str "\n\t\t " . brack code
>	. (if not (null toks) then
>		  str ";\n  reduction " 
> 		. interleave " " (map str (take (length toks) (repeat "_")))
>		. str " = notHappyAtAll "
>	   else
>		id)
>	. str "}"

>     | otherwise
> 	= mkReductionHdr (showInt lt) "happyReduce "  id
>	. str "(" . interleave " :\n\t" tokPatterns
>	. str "happyRest)\n\t = "
>	. makeAbsSynCon nt . str "\n\t\t " . brack code . str " : happyRest"
>	. defaultCase
>	. str "}"

>       where 
>		isMonadProd ('%' : code) = True
>		isMonadProd _ = False
> 
>		mkReductionHdr lt s arg = 
>			mkReductionFun i . str " = "
>			. str s . lt . strspace . showInt nt . arg
>			. str " reduction where {\n  reduction\n\t"
> 
>		defaultCase = if not (null toks)
>              			then str ";\n  reduction _ = notHappyAtAll "
>              			else id
> 
>		tokPatterns = reverse (zipWith tokPattern [1 :: Int ..] toks)
> 
>		tokPattern n _ | n `notElem` vars_used = str "_"
>             	tokPattern n t | t >= 0 && t < first_term
>	      		= str "("
>			. makeAbsSynCon t . str "  "
>			. mkHappyVar n
>			. str ")"
>		tokPattern n t
>			= str "(HappyTerminal " 
>			. mkHappyTerminalVar n t
>			. str ")"
> 
>		(code,vars_used) = expandVars sem
> 
>		lt = length toks

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
>	    	_ ->  str "action "	. eofTok . str " " . eofTok . eofError
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
>	  removeDollorDollor xs = case mapDollorDollor xs of
>				   Nothing -> xs
>				   Just fn -> fn "_"
>    mkHappyTerminalVar :: Int -> Int -> String -> String
>    mkHappyTerminalVar i t = 
>     case tok_str_fn of
>	Nothing -> pat 
>	Just fn -> brack (fn (pat []))
>     where
>	  tok_str_fn = case lookup t token_rep of
>		      Nothing -> Nothing
>		      Just str -> mapDollorDollor str
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

>    produceStateFunction goto ASSOC(state, acts)
> 	= foldr (.) id (map produceActions (assocs acts))
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
>	      produceActions ASSOC(t, LR'Fail{-'-}) = id
>	      produceActions ASSOC(t, action@(LR'Reduce{-'-} _))
>	      	= if default_act == LR'Fail
>			then   actionFunction t
>			     . mkAction action . str "\n"
>			else id
>	      produceActions ASSOC(t, action)
>	      	= actionFunction t
>		. mkAction action . str "\n"
>		
>	      produceGotos ASSOC(t, Goto i)
>	        = actionFunction t
>		. str "happyGoto " . mkActionName i . str "\n"
>	      produceGotos ASSOC(t, NoGoto) = id
>	      
>	      actionFunction t
>	      	= mkActionName state . strspace
>		. ('(' :) . showInt t
>		. str ") = "
>		
> 	      default_act = getDefault (elems acts)

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
>    (first_state, last_state) 		= bounds action
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
>			[ ASSOC(a, fn a b) | ASSOC(a, b) <- assocs nt_types ]
>     where
>	fn n Nothing = n
>	fn n (Just a) = case lookup a assoc_list of
>			  Just v -> v
>			  Nothing -> error ("cant find an item in list")
>	assoc_list = [ (b,a) | ASSOC(a, Just b) <- assocs nt_types ]

>    makeAbsSynCon = mkAbsSynCon nt_types_index

>    mkMonadTy s = case monad of
>			Nothing -> s
>			Just (ty,_,_) -> str (ty++"(") . s . str ")"

>    mkMonadThen a b c  =
>		case monad of
>		    Nothing -> c
>		    Just (_,tn,r) -> str tn . str " (" . a . str ") (\\"
>					      . b  . str " -> " . c . str ")"

>    mkMonadReturn s = case monad of
>			Nothing -> s
>			Just (_,_,r) -> str (r++"(") . s . str ")"

>    produceMonadStuff =
>	  str "happyThen = "
>	. (case monad of
>	    Nothing -> str "\\m k -> k m"
>	    Just (_,tn,_) -> str tn)
>	. str "\n"
>	. str "happyReturn = "
>	. (case monad of
>	    Nothing -> case lexer of
>			  Nothing -> str "\\a tks -> a"
>			  _       -> str "\\a -> a"
>	    Just (_,_,rtn) -> str rtn)
>	. str "\n"

>    reduceArrElem n
>      = if opt1_2 then
>		str "\t" . shows n . str " := "
>	      . str "happyReduce_" . shows n
>	 else
>		str "\t(" . shows n . str " , "
>      	      . str "happyReduce_" . shows n . str ")"

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

> mkGoto (Goto i)		= str "happyGoto " . mkActionName i

> mkActionName i		= str "action_" . shows i

> getDefault actions
> 	| (not . null) reduces && all (== (head reduces)) (tail reduces)
>		= head reduces
>	| otherwise = LR'Fail
>   where reduces = [ act | act@(LR'Reduce _) <- actions ]
>   		    ++ [ act | (LR'Multiple _ act@(LR'Reduce _)) <- actions ]

-----------------------------------------------------------------------------
Replace all the $n variables with happy_vars, and return a list of all the
vars used in this piece of code.

> expandVars :: String -> (String,[Int])
> expandVars [] = ("",[])
> expandVars ('$':r) 
>	| isDigit (head r) = ("happy_var_" ++ num ++ code, read num : vars)
>	| otherwise = error ("Illegal attribute: $" ++ [head r] ++ "\n")
>    where
>	(num,rest)  = span isDigit r
>	(code,vars) = expandVars rest
> expandVars (c:r) = (c:code,vars)
>    where
>	(code,vars) = expandVars r

-----------------------------------------------------------------------------
Misc.

> comment = 
> 	"-- parser produced by Happy Version " ++ version ++ "\n\n"

> str = showString
> strspace = str " "
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' s = foldr1 (\a b -> a . str s . b) 

> mkAbsSynCon fx t    	= str "HappyAbsSyn"   . shows (fx ! t)
> mkHappyVar n     	= str "happy_var_"    . shows n
> mkReductionFun n 	= str "happyReduce_"  . shows n

> specReduceFun 	= (<= (3 :: Int))

> maybestr (Just s)	= str s
> maybestr _		= id

> mapDollorDollor :: String -> Maybe (String -> String)
> mapDollorDollor "" = Nothing
> mapDollorDollor ('$':'$':r) = -- only map first instance
>    case mapDollorDollor r of
>	Just fn -> error "more that one $$ in pattern"
>	Nothing -> Just (\ s -> s ++ r)
> mapDollorDollor (c:r) =
>    case mapDollorDollor r of
>	Just fn -> Just (\ s -> c : fn s)
>	Nothing -> Nothing

> brack s = str ('(' : s) . str ")"

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
>		(if comma then str "," else id)
>		. interleave' "," (map shows (take l (repeat x)))
>		. rle_to_str xs True
