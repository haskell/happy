-----------------------------------------------------------------------------
$Id: Grammar.lhs,v 1.13 2000/07/12 16:21:44 simonmar Exp $

The Grammar data type.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is our mid-section datatype

> module Grammar (
> 	Name, isEmpty, 
>	
>	Production,  Productions, Terminals, NonTerminals,
>	Grammar(..), mangler, fixDir, getTerm, checkRules, 
>	
>	LRAction(..), ActionTable, Goto(..), GotoTable, Priority(..),
>       Assoc(..),
>	
>	errorName, errorTok, startName, startTok, dummyTok,
>	firstNT, eofName, epsilonTok
>	) where

> import GenUtils
> import AbsSyn

> import Array
> import IOExts

epsilon		= 0
error		= 1
dummy		= 2
%start 		= 3
non-terminals 	= 4..n
terminals 	= n..m
%eof 		= m

These numbers are deeply magical, change at your own risk.  Several
other places rely on these being arranged as they are, including
ProduceCode.lhs and the various HappyTemplates.

> type Name = Int

> type Production = (Name,[Name],String,Priority)
> type Productions = Array Int Production
> type Terminals = [Name]
> type NonTerminals = [Name]

> data Grammar 
>       = Grammar {
>		productions 	:: [Production],
>		lookupProdNo 	:: Int -> Production,
>		lookupProdsOfName :: Name -> [Int],
>               directives 	:: [Directive Name],
>               terminals 	:: Terminals,
>               non_terminals 	:: NonTerminals,
>		types 		:: Array Int (Maybe String),
>               token_names 	:: Array Int String,
>		first_term 	:: Name,
>               eof_term	:: Name,
>               priorities      :: [(Name,Priority)]
>	}

> data Assoc = LeftAssoc | RightAssoc | None
> data Priority = No | Prio Assoc Int

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

#ifdef DEBUG

> instance Show Grammar where
>       showsPrec _ (Grammar 
>		{ productions		= p
>               , directives		= d
>               , terminals		= ts
>               , non_terminals		= nts
>		, types			= tys
>               , token_names		= e
>		, first_term		= ft
>               , eof_term		= eof
>	 	})
>	 =      shows p . showString "\n" .
>               shows d . showString "\n" .
>               shows ts . showString "\n" .
>               shows nts . showString "\n" .
>               shows tys . showString "\n" .
>               shows e . showString "\n" .
>		shows ft . showString "\n" .
>               shows eof . showString "\n"

#endif

> startName = "%start"			-- Token 2
> eofName   = "%eof"			
> errorName = "error"			-- Token 1

> startTok, dummyTok, firstNT, errorTok, epsilonTok :: Name
> firstNT    = startTok+1
> startTok   = 3
> dummyTok   = 2
> errorTok   = 1
> epsilonTok = 0

> isEmpty :: Name -> Bool
> isEmpty 0 = True
> isEmpty _ = False

%-----------------------------------------------------------------------------
The Mangler.

We are allowed to use the facts that the start symbol is always
non-terminal zero, and the first non-terminal in the grammar file is
non-terminal 1.

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

>	checkRules ([n | (n,_,_) <- rules]) "" []	   `thenE` \nts  ->

>	let
>       term     = concat (map getTerm dirs) ++ [eofName]
>       nonterm' = [ firstNT .. l_nt+firstNT-1 ]
>       term'    = [ l_nt+firstNT .. l_nt+l_t+firstNT-1 ]
>       env      = (errorTok, errorName) :
>		   (startTok, startName) :
>		   zip (nonterm'++term') (nts ++ term)
>	l_nt     = length nts
>	l_t      = length term

>       priodir = zip [1..] (getPrios dirs)
>
>       prios = [ (name,mkPrio i dir)
>               | (i,dir) <- priodir
>               , nm <- AbsSyn.getNames dir
>		, name <- [a|(a,r)<-env,r==nm]
>		]

>       prioByString = [ (name, mkPrio i dir)
>                      | (i,dir) <- priodir
>                      , name <- AbsSyn.getNames dir
>                      ]


>       mapToName str = 
>             case [ a | (a,r) <- env, r == str ] of
>                [a] -> Succeeded a
>                []  -> Failed ["unknown identifer `" ++ str ++ "'"]
>                _   -> Failed ["multiple use of `" ++ str ++ "'"]

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
>               Nothing -> case filter (flip elem term') lhs of
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
>	tys   = listArray' (firstNT, l_nt+firstNT-1) 
>			[ ty | (nm,_,ty) <- rules ]

>	env_array :: Array Int String
>	env_array = array (errorTok, l_nt+l_t+firstNT-1) env
>	in

>       foldr (mightFails (fixDir mapToName)) (Succeeded []) dirs
>							`thenE` \dirs' ->

>	let
>	   ass = combinePairs [ (a,no) | ((a,_,_,_),no) <- zip prod [0..] ]
>	   arr = array (startTok, length ass - 1 + startTok) ass

>	   first_term = head term'

>	   lookup_prods :: Name -> [Int]
>	   lookup_prods x | x >= startTok && x < first_term = arr ! x
>	   lookup_prods _ = error "looking up production failure"
>
>	   prod = (startTok, [firstNT], "no code", No) : concat rules'

>	in

>	Succeeded (Grammar {
>		productions 		= prod,
>		lookupProdNo	  	= (listArray' (0,length prod-1) prod !),
>		lookupProdsOfName 	= lookup_prods,
>               directives		= dirs',
>               terminals		= (errorTok : term'),
>               non_terminals		= (startTok : nonterm'),
>		types			= tys,
>               token_names		= env_array,
>		first_term		= first_term,
>               eof_term		= last term',
>               priorities              = prios
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

This is horrible.

> fixDir f (TokenType str)    = Succeeded (TokenType str)
> fixDir f (TokenName str)    = Succeeded (TokenName str)
> fixDir f (TokenLexer lex eof) = Succeeded (TokenLexer lex eof)
> fixDir f (TokenSpec stuff)  
> 	= foldr (mightFails (\(a,b) -> f a `thenE` \a -> Succeeded (a,b))) 
>			(Succeeded []) stuff `thenE` \stuff' ->
>	  Succeeded (TokenSpec stuff')
> fixDir f (TokenMonad ty tn rt) = Succeeded (TokenMonad ty tn rt)
> fixDir f (TokenNonassoc str) = Succeeded (TokenNonassoc str)
> fixDir f (TokenRight str)    = Succeeded (TokenRight str)
> fixDir f (TokenLeft str)     = Succeeded (TokenLeft str)

> getTerm (TokenSpec stuff) = map fst stuff
> getTerm _                 = []

So is this.

> checkRules (name:rest) above nonterms
>       | name == above = checkRules rest name nonterms
>       | name `elem` nonterms 
>		= Failed ["Multiple rules for `" ++ name ++ "'"]
>       | otherwise = checkRules rest name (name : nonterms)

> checkRules [] _ nonterms = Succeeded (reverse nonterms)

%-----------------------------------------------------------------------------
\subsection{Internal Reduction Datatypes}

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

