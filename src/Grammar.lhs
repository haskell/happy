-----------------------------------------------------------------------------
$Id: Grammar.lhs,v 1.15 2000/12/03 16:21:50 simonmar Exp $

The Grammar data type.

(c) 1993-1996 Andy Gill, Simon Marlow
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

#ifdef DEBUG

> import IOExts

#endif

> type Name = Int

> type Production = (Name,[Name],String,Priority)

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

> instance Ord Priority where
>   No <= _ = True
>   Prio _ _ <= No = False
>   Prio _ i <= Prio _ j = i <= j

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

> {- m `parE` k 
> 	= case m of
>		Failed e    -> case k (error "parE") of
>				Failed e' -> Failed (e ++ e')
>				Succeeded b -> Failed e
>		Succeeded a -> case k a of
>				Failed e -> Failed e
>				Succeeded b -> Succeeded b -}

> mangler :: AbsSyn -> MaybeErr Grammar [String]
> mangler (AbsSyn hd dirs rules tl) = 

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

>	starts	    = getParserNames dirs
>	start_strs  = map (\n -> startName++'_':show n) [1..n_starts :: Int]

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

>	foldr (mightFails lookupStart) (Succeeded []) starts
>					`thenE` \ start_toks ->

>	let
>	parser_names = [ s | TokenName s _ <- starts ]
>	start_prods = zipWith (\nm tok -> (nm, [tok], "no code", No))
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
>   	  = mapToName nt				       `thenE` \nt' ->
>	    foldr (mightFails transProds) (Succeeded []) prods `thenE` \prods ->
>	    foldr (mightFails (finishRule nt')) (Succeeded []) prods
>
>	finishRule nt (lhs,code,prec)
>	  = case mkPrec lhs prec of
>		Left s  -> Failed ["Undeclared precedence token: " ++ s]
>		Right p -> Succeeded (nt, lhs, code, p)
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

> 	transProds (lhs, code, line, prec)
>   	  = case foldr (mightFails mapToName) (Succeeded []) lhs of
>   		Succeeded lhs' -> Succeeded (lhs', code, prec)
>		Failed s -> Failed (map (("line " ++ show line ++ ": ") ++) s)

>	in

>	foldr (mightFails transRule) (Succeeded []) rules  `thenE` \rules' ->

>	let
>	tys = listArray' (first_nt, last_nt) [ ty | (nm,_,ty) <- rules ]

>	env_array :: Array Int String
>	env_array = array (errorTok, last_t) name_env
>	in

Get the token specs in terms of Names.

>	let 
>	fixTokenSpec (a,b) = mapToName a `thenE` \a -> Succeeded (a,b)
>	in
>       foldr (mightFails fixTokenSpec) (Succeeded []) (getTokenSpec dirs)
>						`thenE` \tokspec ->

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

> mightFails 
> 	:: (a -> MaybeErr b [c]) -> a
> 	-> MaybeErr [b] [c] 
>	-> MaybeErr [b] [c]

> mightFails f a b
> 	= combine (f a) b
>	where 
>		combine (Succeeded a) (Succeeded b)	= Succeeded (a:b)
> 		combine (Succeeded a) (Failed sb)	= Failed sb
> 		combine (Failed sa) (Succeeded b)	= Failed sa
> 		combine (Failed sa) (Failed sb) 	= Failed (sa ++ sb)

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
