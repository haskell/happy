-----------------------------------------------------------------------------
$Id: Grammar.lhs,v 1.19 2001/09/24 15:48:54 simonmar Exp $

The Grammar data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is our mid-section datatype

> module Grammar (
> 	Name, isEmpty, 
>	
>	Production, Grammar(..), mangler,
>	
>	LRAction(..), ActionTable, Goto(..), GotoTable, Priority(..),
>       Assoc(..),
>	
>	errorName, errorTok, startName, firstStartTok, dummyTok,
>	eofName, epsilonTok
>	) where

> import GenUtils
> import AbsSyn

> import Array
> import Char

#ifdef DEBUG

> import IOExts

#endif

> type Name = Int

> type Production = (Name,[Name],(String,[Int]),Priority)

> data Grammar 
>       = Grammar {
>		productions 	  :: [Production],
>		lookupProdNo 	  :: Int -> Production,
>		lookupProdsOfName :: Name -> [Int],
>               token_specs 	  :: [(Name,String)],
>               terminals 	  :: [Name],
>               non_terminals 	  :: [Name],
>		starts		  :: [(String,Name,Name)],
>		types 		  :: Array Int (Maybe String),
>               token_names 	  :: Array Int String,
>		first_nonterm	  :: Name,
>		first_term 	  :: Name,
>               eof_term	  :: Name,
>               priorities        :: [(Name,Priority)],
>		token_type	  :: String,
>		monad		  :: Maybe (String,String,String),
>		lexer		  :: Maybe (String,String)
>	}

#ifdef DEBUG

> instance Show Grammar where
>       showsPrec _ (Grammar 
>		{ productions		= p
>		, token_specs		= t
>               , terminals		= ts
>               , non_terminals		= nts
>		, starts		= starts
>		, types			= tys
>               , token_names		= e
>		, first_nonterm		= fnt
>		, first_term		= ft
>               , eof_term		= eof
>	 	})
>	 = showString "productions = "     . shows p
>        . showString "\ntoken_specs = "   . shows t
>        . showString "\nterminals = "     . shows ts
>        . showString "\nnonterminals = "  . shows nts
>        . showString "\nstarts = "        . shows starts
>        . showString "\ntypes = "         . shows tys
>        . showString "\ntoken_names = "   . shows e
>	 . showString "\nfirst_nonterm = " . shows fnt
>	 . showString "\nfirst_term = "    . shows ft
>        . showString "\neof = "           . shows eof
>	 . showString "\n"

#endif

> data Assoc = LeftAssoc | RightAssoc | None

#ifdef DEBUG

>	deriving Show

#endif

> data Priority = No | Prio Assoc Int

#ifdef DEBUG

>	deriving Show

#endif

> instance Eq Priority where
>   No == No = True
>   Prio _ i == Prio _ j = i == j
>   _ == _ = False

> mkPrio :: Int -> Directive a -> Priority
> mkPrio i (TokenNonassoc _) = Prio None i
> mkPrio i (TokenRight _) = Prio RightAssoc i
> mkPrio i (TokenLeft _) = Prio LeftAssoc i
> mkPrio i _ = error "Panic: impossible case in mkPrio"

-----------------------------------------------------------------------------
-- Magic name values

All the tokens in the grammar are mapped onto integers, for speed.
The namespace is broken up as follows:

epsilon		= 0
error		= 1
dummy		= 2
%start 		= 3..s
non-terminals 	= s..n
terminals 	= n..m
%eof 		= m

These numbers are deeply magical, change at your own risk.  Several
other places rely on these being arranged as they are, including
ProduceCode.lhs and the various HappyTemplates.

Unfortunately this means you can't tell whether a given token is a
terminal or non-terminal without knowing the boundaries of the
namespace, which are kept in the Grammar structure.

In hindsight, this was probably a bad idea.

> startName = "%start" -- with a suffix, like %start_1, %start_2 etc.
> eofName   = "%eof"			
> errorName = "error"
> dummyName = "%dummy"  -- shouldn't occur in the grammar anywhere

> firstStartTok, dummyTok, errorTok, epsilonTok :: Name
> firstStartTok   = 3
> dummyTok        = 2
> errorTok    	  = 1
> epsilonTok 	  = 0

> isEmpty :: Name -> Bool
> isEmpty n | n == epsilonTok = True
>	    | otherwise       = False

-----------------------------------------------------------------------------
-- The Mangler

This bit is a real mess, mainly because of the error message support.

> m `thenE` k 
> 	= case m of
>		Failed e    -> Failed e
>		Succeeded a -> case k a of
>				Failed e -> Failed e
>				Succeeded b -> Succeeded b

> m `parE` k 
> 	= case m of
>		Failed e    -> case k (error "parE") of
>				Failed e' -> Failed (e ++ e')
>				Succeeded _ -> Failed e
>		Succeeded a -> case k a of
>				Failed e -> Failed e
>				Succeeded b -> Succeeded b

> parEs [] = Succeeded []
> parEs (x:xs) = x `parE` \x' ->
>		 parEs xs `parE` \xs' ->
>		 Succeeded (x':xs')

> failMap :: (b -> c) -> MaybeErr a [b] -> MaybeErr a [c]
> failMap f e = case e of
>   		  Succeeded a -> Succeeded a
>		  Failed s -> Failed (map f s)


> mangler :: FilePath -> AbsSyn -> MaybeErr Grammar [String]
> mangler file (AbsSyn hd dirs rules tl) = 

>	  -- add filename to all error messages
>	failMap (\s -> file ++ ": " ++ s) $

>	checkRules ([n | (n,_,_) <- rules]) "" [] `thenE` \nonterm_strs  ->

>	let

>       terminal_strs  = concat (map getTerm dirs) ++ [eofName]

>	n_starts   = length starts
>	n_nts      = length nonterm_strs
>	n_ts       = length terminal_strs
>	first_nt   = firstStartTok + n_starts
>	first_t    = first_nt + n_nts
>	last_start = first_nt - 1
>	last_nt    = first_t  - 1
>	last_t     = first_t + n_ts - 1

>	start_names    = [ firstStartTok .. last_start ]
>       nonterm_names  = [ first_nt .. last_nt ]
>       terminal_names = [ first_t .. last_t ]

>	starts	    = case getParserNames dirs of
>			[] -> [TokenName "happyParse" Nothing]
>			ns -> ns
>
>	start_strs  = [ startName++'_':p  | (TokenName p _) <- starts ]

Build up a mapping from name values to strings.

>       name_env = (errorTok, errorName) :
>		   (dummyTok, dummyName) :
>		   zip start_names    start_strs ++
>		   zip nonterm_names  nonterm_strs ++
>		   zip terminal_names terminal_strs

>	lookupName :: String -> [Name]
>	lookupName n = [ t | (t,r) <- name_env, r == n ]

>       mapToName str = 
>             case lookupName str  of
>                [a] -> Succeeded a
>                []  -> Failed ["unknown identifier `" ++ str ++ "'"]
>                _   -> Failed ["multiple use of `" ++ str ++ "'"]

Start symbols...

>		-- default start token is the first non-terminal in the grammar
>	lookupStart (TokenName s Nothing)  = Succeeded first_nt
>	lookupStart (TokenName s (Just n)) = mapToName n
>	in

>	parEs (map lookupStart starts)	`thenE` \ start_toks ->

>	let
>	parser_names = [ s | TokenName s _ <- starts ]
>	start_prods = zipWith (\nm tok -> (nm, [tok], ("no code",[]), No))
>			 start_names start_toks

Deal with priorities...

>       priodir = zip [1..] (getPrios dirs)
>
>       prios = [ (name,mkPrio i dir)
>               | (i,dir) <- priodir
>               , nm <- AbsSyn.getPrioNames dir
>		, name <- lookupName nm
>		]

>       prioByString = [ (name, mkPrio i dir)
>                      | (i,dir) <- priodir
>                      , name <- AbsSyn.getPrioNames dir
>                      ]

Translate the rules from string to name-based.

> 	transRule (nt, prods, ty)
>   	  = mapToName nt `thenE` \nt' ->
>	    parEs (map (finishRule nt') prods)
>
>	finishRule nt (lhs,code,line,prec)
>	  = failMap (addLine line) $
>	    checkCode (length lhs) code `parE`  \code' ->
>	    parEs (map mapToName lhs) 	`thenE`  \lhs' ->
>	    case mkPrec lhs' prec of
>		Left s  -> Failed ["Undeclared precedence token: " ++ s]
>		Right p -> Succeeded (nt, lhs', code', p)
>
>       mkPrec :: [Name] -> Maybe String -> Either String Priority
>       mkPrec lhs prio =
>             case prio of
>               Nothing -> case filter (flip elem terminal_names) lhs of
>                            [] -> Right No
>                            xs -> case lookup (last xs) prios of
>                                    Nothing -> Right No
>                                    Just p  -> Right p
>               Just s -> case lookup s prioByString of
>                           Nothing -> Left s
>                           Just p -> Right p
>	in

>	parEs (map transRule rules) `thenE` \rules' ->

>	let
>	tys = listArray' (first_nt, last_nt) [ ty | (nm,_,ty) <- rules ]

>	env_array :: Array Int String
>	env_array = array (errorTok, last_t) name_env
>	in

Get the token specs in terms of Names.

>	let 
>	fixTokenSpec (a,b) = mapToName a `thenE` \a -> Succeeded (a,b)
>	in
>       parEs (map fixTokenSpec (getTokenSpec dirs)) `thenE` \tokspec ->

>	let
>	   ass = combinePairs [ (a,no)
>			      | ((a,_,_,_),no) <- zip productions [0..] ]
>	   arr = array (firstStartTok, length ass - 1 + firstStartTok) ass

>	   lookup_prods :: Name -> [Int]
>	   lookup_prods x | x >= firstStartTok && x < first_t = arr ! x
>	   lookup_prods _ = error "lookup_prods"
>
>	   productions = start_prods ++ concat rules'
>	   prod_array  = listArray' (0,length productions-1) productions

>	in

>	Succeeded (Grammar {
>		productions 	  = productions,
>		lookupProdNo   	  = (prod_array !),
>		lookupProdsOfName = lookup_prods,
>               token_specs	  = tokspec,
>               terminals	  = errorTok : terminal_names,
>               non_terminals	  = start_names ++ nonterm_names,
>				  	-- INCLUDES the %start tokens
>		starts		  = zip3 parser_names start_names start_toks,
>		types		  = tys,
>               token_names	  = env_array,
>		first_nonterm	  = first_nt,
>		first_term	  = first_t,
>               eof_term	  = last terminal_names,
>               priorities        = prios,
>		monad		  = getMonad dirs,
>		lexer		  = getLexer dirs,
>		token_type	  = getTokenType dirs
>	})

For combining actions with possible error messages.

> addLine :: Int -> String -> String
> addLine l s = show l ++ ": " ++ s

> getTerm (TokenSpec stuff) = map fst stuff
> getTerm _                 = []

So is this.

> checkRules (name:rest) above nonterms
>       | name == above = checkRules rest name nonterms
>       | name `elem` nonterms 
>		= Failed ["Multiple rules for `" ++ name ++ "'"]
>       | otherwise = checkRules rest name (name : nonterms)

> checkRules [] _ nonterms = Succeeded (reverse nonterms)

-----------------------------------------------------------------------------
-- Check for every $i that i is <= the arity of the rule.

-- At the same time, we collect a list of the variables actually used in this
-- code, which is used by the backend.

> checkCode :: Int -> String -> MaybeErr (String, [Int]) [String]
> checkCode arity code = go code "" []
>   where go code acc used =
>           case code of
>		[] -> Succeeded (reverse acc, used)
>	
>		'"'  :r    -> case reads code :: [(String,String)] of
>				 []      -> go r ('"':acc) used
>				 (s,r):_ -> go r (reverse (show s) ++ acc) used
>		a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc) used
>		'\'' :r    -> case reads code :: [(Char,String)] of
>				 []      -> go r ('\'':acc) used
>				 (c,r):_ -> go r (reverse (show c) ++ acc) used
>		'\\':'$':r -> go r ('$':acc) used
>		'$':r@(i:_) | isDigit i -> 
>			case reads r :: [(Int,String)] of
>			  (j,r):_ -> 
>			     if j > arity 
>			   	  then Failed [ '$': show j ++ " out of range" ] 
>				 	`parE` \_ -> go r acc used
>			   	  else go r (reverse (mkHappyVar j) ++ acc) 
>					 (j : used)
>			  
>		c:r  -> go r (c:acc) used

> mkHappyVar n 	= "happy_var_" ++ show n

-----------------------------------------------------------------------------
-- Internal Reduction Datatypes

> data LRAction = LR'Shift Int Priority -- state number and priority
>               | LR'Reduce Int Priority-- rule no and priority
>               | LR'Accept             -- :-)
>               | LR'Fail               -- :-(
>               | LR'MustFail           -- :-(
>		| LR'Multiple [LRAction] LRAction	-- conflict
>       deriving(Eq

#ifdef DEBUG

>	,Show

#endif

>	)	

> type ActionTable = Array Int{-state-} (Array Int{-terminal#-} LRAction)

 instance Text LRAction where 
   showsPrec _ (LR'Shift i _)  = showString ("s" ++ show i)
   showsPrec _ (LR'Reduce i _) 
       = showString ("r" ++ show i)
   showsPrec _ (LR'Accept)     = showString ("acc")
   showsPrec _ (LR'Fail)       = showString (" ")
 instance Eq LRAction where { (==) = primGenericEq } 

> data Goto = Goto Int | NoGoto 
>       deriving(Eq

#ifdef DEBUG

>	,Show

#endif

>	)	

> type GotoTable = Array Int{-state-} (Array Int{-nonterminal #-} Goto)
