%-----------------------------------------------------------------------------
ProduceCode.lhs
(c) Andy Gill, Simon Marlow 1993
%-----------------------------------------------------------------------------

> module ProduceCode (produceParser, str, interleave, interleave') where

> import Version		( version )
> import GenUtils
> import AbsSyn
> import Grammar
> import Target			( Target(..) )

%-----------------------------------------------------------------------------
Produce the complete output file.

> produceParser :: GrammarInfo 			-- grammar info
>		-> ActionTable 			-- action table
>		-> GotoTable 			-- goto table
>		-> Maybe String			-- newline thing
>		-> [(Int,String)]		-- token reps
>		-> String			-- token type
>		-> Array Int (Maybe String)     -- types of NonTerminals
>		-> String			-- parser name
>		-> Maybe String			-- module header
>		-> Maybe String			-- module trailer
>		-> Target			-- type of code required
>		-> String

> produceParser
> 	(GrammarInfo prods lookupProd lookupProdNos nonterms terms eof) 
>	 action goto newline token_rep token_type nt_types
> 	 name module_header module_trailer target = (
>	 
> 	  str comment
>	. maybestr module_header . str "\n"
> 	. produceAbsSynDecl
>    	. produceTypes
>	. produceActionTable target
>	. produceReductions
>	. produceTokenConverter
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
>	. str (unwords [ "t" ++ show n | n := Nothing <- assocs nt_types ])
>	. str "\n\t= HappyTerminal (" . str token_type . str ")\n"
>	. interleave "\n" 
>         [ str "\t| " . 
>             mkAbsSynCon n . 
>             (case ty of 
>                 Nothing -> showString " t" . shows n
>                 Just t  -> showString " ( " . showString t . showString " )")
>         | n := ty <- assocs nt_types ]
>	. str "\n"

%-----------------------------------------------------------------------------
Type declarations of the form:

type HappyReduction a b = ....
action_0, action_1 :: Int -> HappyReduction a b 
reduction_1, ...   :: HappyReduction a b 

These are only generated if types for *all* rules are given (and not for array
based parsers -- types aren't as important there).

>    produceTypes | target == TargetArrayBased = id

>     | all maybeToBool (elems nt_types) =
>     let (Just res_type) = nt_types ! 1
>     in
>       str "type HappyReduction = \n\t"
>     . str "   "
>     . intMaybeHash
>     . str " \n\t-> ("
>     . str token_type 
>     . str ") \n\t-> HappyState ("
>     . str token_type 
>     . str ") ([HappyAbsSyn] -> ("
>     . str res_type 
>     . str ")) \n\t-> Int \n\t-> ["
>     . str token_type 
>     . str "] \n\t-> [HappyState ("
>     . str token_type 
>     . str ") ([HappyAbsSyn] -> ("
>     . str res_type 
>     . str "))] \n\t-> [HappyAbsSyn] \n\t-> ("
>     . str res_type 
>     . str ")\n\n"
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

%-----------------------------------------------------------------------------
Next, the reduction functions.   Each one has the following form:

happyReduce_n_m = happyReduce n m reduction where {
   reduction (
	(HappyAbsSynX  | HappyTerminal) happy_var_1 :
	..
	(HappyAbsSynX  | HappyTerminal) happy_var_q :
	happyRest)
	happy_var_lineno = HappyAbsSynY
		( <<user supplied string>> ) : happyRest
	; reduction _ _ = notHappyAtAll n m

where n is the non-terminal number, and m is the rule number.

>    produceReductions =
> 	interleave "\n" (zipWith produceReduction (tail prods) [ 1 :: Int .. ])

>    produceReduction (NonTerminal nt, toks, sem) i
> 	= mkReductionFun i
>	. str " = "
>	. reduceFun lt
>	. str " reduction where {\n  reduction (\n\t"
>	. interleave " :\n\t" (reverse (zipWith tokPattern [1 :: Int ..] toks))
>	. str "happyRest)\n\thappy_var_lineno = "
>	. mkAbsSynCon nt . str "\n\t\t( " . code
>	. str " ) : happyRest"
>	. (if not (null toks) 
>        	then str ";\n  reduction _ _ = notHappyAtAll " . shows i  
>          	else id) 
>	. str "}\n"

>       where 
>		tokPattern n _ | n `notElem` vars_used = str "_"
>		tokPattern n (Terminal t)    
>			= str "HappyTerminal " . mkHappyTerminalVar n t
>             	tokPattern n (NonTerminal t) 
>	      		= mkAbsSynCon t . str "  " . mkHappyVar n
>		(code,vars_used) = expandVars (reverse sem) id []

>		lt = length toks

>		reduceFun n 
>		   | specReduceFun n 
>			= str "specHappyReduce_" . shows n . strspace 
>			. showInt nt
>		   | otherwise
>		   	= str "happyReduce " . showInt nt . strspace 
>			. showInt n

%-----------------------------------------------------------------------------
The token conversion function.

>    produceTokenConverter
>    	= str "happyNewToken action ln []\n\t"
>    	. (case target of
>	    TargetArrayBased ->
>	   	  str "= happyDoAction "
>		. showInt (tokIndex (nameToInt eof))
>		. str " (error \"reading EOF!\") action ln []\n\n"
>	    _ ->  str "= action "
>		. showInt (tokIndex (nameToInt eof))
>		. str " "
>		. showInt (tokIndex (nameToInt eof))
>		. str " (error \"reading EOF!\") (HappyState action) ln []\n\n")
>
>       . str "happyNewToken action ln (tk:tks) = case tk of\n"
>	. interleave "\n" (map doToken token_rep)
>	. (case newline of
>		Nothing -> id
>		Just s ->   str "\t" 
>			  . str s
>			  . str " -> happyNewToken action (ln + 1) tks\n")
>	. (case target of
>	    TargetArrayBased 
>		-> str "  where cont i = happyDoAction i tk action ln tks\n\n"
>	    _   -> str "  where cont i = action i i tk (HappyState action) ln tks\n\n")

>	where 
>	  doToken (i,tok) 
>		= str "\t" 
>		. str (removeDollorDollor tok)
>		. str " -> cont " 
>		. showInt (tokIndex i)
>	  removeDollorDollor xs = case mapDollorDollor xs of
>				   Nothing -> xs
>				   Just fn -> fn "_"
>    mkHappyTerminalVar :: Int -> Int -> String -> String
>    mkHappyTerminalVar i t = 
>     (case tok_str_fn of
>	Nothing -> pat 
>	Just fn -> (\ s -> "(" ++ fn (pat []) ++ ")" ++ s))
>     where
>	  tok_str_fn = case assocMaybe token_rep t of
>		      Nothing -> Nothing
>		      Just str -> mapDollorDollor str
>	  pat = mkHappyVar i

>    tokIndex 
>	= case target of
>		TargetGhc	 -> id
>		TargetHaskell 	 -> id
>		TargetArrayBased -> \i -> i - first_terminal


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

>    produceStateFunction goto (state := acts)
> 	= foldr (.) id (map produceActions (assocs acts))
>	. foldr (.) id (map produceGotos   (assocs gotos))
>	. mkActionName state
>	. str " _ = "
>	. mkAction default_act
>	. str "\n"
>	. possibleTcHackForGhc
>	. str "\n"
>
>	where gotos = goto ! state
>	
>	      produceActions (t := LR'Fail) = id
>	      produceActions (t := action@(LR'Reduce _))
>	      	= if default_act == LR'Fail
>			then   actionFunction t
>			     . mkAction action . str "\n"
>			else id
>	      produceActions (t := action)
>	      	= actionFunction t
>		. mkAction action . str "\n"
>		
>	      produceGotos (t := Goto i)
>	        = actionFunction t
>		. str "happyGoto " . mkActionName i . str "\n"
>	      produceGotos (t := NoGoto) = id
>	      
>	      actionFunction t
>	      	= mkActionName state . strspace
>		. showInt t
>		. str " = "
>		
>	      possibleTcHackForGhc = case target of
>	      	TargetGhc ->   mkActionName state 
>			     . str " 0# = error \"\"\n"
>		_	  -> id
>		
> 	      default_act = getDefault (elems acts)

action array indexed by (terminal * last_state) + state

>    produceActionArray
>   	= str "happyActionArr :: Array Int Int\n"
>	. str "happyActionArr = listArray (0,"
>		. shows (n_terminals * n_states)
>		. str ") [ \n"
>	. interleave' ",\n" (
>		map (str "\t" .) (
>		map (interleave' ", ") (
>		map actionArrElems (elems action))))
>	. str "\n\t]\n\n"
>	
>    (first_state, last_state) 		= bounds action
>    (first_terminal, last_terminal) 	= bounds (action ! 0)
>    n_states = last_state + 1
>    n_terminals = last_terminal - first_terminal + 1
>    n_nonterminals = first_terminal - 1
>
>    actionArrElems actions = map actionVal (elems actions)

>    produceGotoArray
>   	= str "happyGotoArr :: Array Int Int\n"
>	. str "happyGotoArr = listArray (0, "
>		. shows ((first_terminal - 1) * n_states)
>		. str ") [\n"
>	. interleave' ",\n" (
>		map (str "\t" .) (
>		map (interleave' ", ") (
>		map gotoArrElems (elems goto))))
>	. str "\n\t]\n\n"

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

> reduceArrElem n
>   	= str "\t" . shows n . str " := "
>	. str "happyReduce_" . shows n

> actionVal (LR'Shift  state) 	= shows (state + 1)
> actionVal (LR'Reduce rule)  	= shows (-(rule + 1))
> actionVal  LR'Accept		= shows (-1 :: Int)
> actionVal  LR'Fail		= shows (0 :: Int)
> actionVal (LR'Multiple _ a)	= actionVal a

> gotoVal (Goto i)		= shows i
> gotoVal NoGoto		= shows (0 :: Int)
  
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

%-----------------------------------------------------------------------------
Replace all the $n variables with happy_vars, and return a list of all the
vars used in this piece of code.

> expandVars [] new vars = (new,vars)
> expandVars ('#':'$':r) new vars
>	= expandVars r (("happy_var_lineno" ++) . new) vars
> expandVars (n  :'$':r) new vars
>	= expandVars r (("happy_var_" ++) . (n :) . new) 
>		((ord n - ord '0') : vars)
> expandVars (c:r) new vars = expandVars r ((c :) . new) vars

%-----------------------------------------------------------------------------
Misc.

> comment = 
> 	"-- parser produced by Happy Version " ++ version ++ "\n\n"

> str = showString
> strspace = str " "
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' s = foldr1 (\a b -> a . str s . b) 

> mkAbsSynCon t    	= str "HappyAbsSyn"   . shows t
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

