-----------------------------------------------------------------------------
$Id: Grammar.lhs,v 1.4 1997/07/16 13:32:35 simonm Exp $

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
>	LRAction(..), ActionTable, Goto(..), GotoTable,
>	
>	GrammarInfo(..),
>	getProds, lookupProdNo, lookupProdsOfName, getNonTerminals,
>	getTerminals, getEOF, getNames, mkProdInfo, getFirstTerm,

>	errorName, errorTok, startName, startTok, eofName, epsilonTok
>	) where

> import GenUtils
> import AbsSyn
> import Array

epsilon		= -2
error		= -1
%start 		= 0
non-terminals 	= 1..n
terminals 	= n..m
%eof 		= m

> type Name = Int

> isEmpty n    = n == epsilonTok

> type Production = (Name,[Name],String)
> type Productions = Array Int Production
> type Terminals = [Name]
> type NonTerminals = [Name]

> data Grammar 
>       = Grammar
>               GrammarInfo
>               [Directive Name]
>               Terminals
>               NonTerminals
>		(Array Int (Maybe String))	-- types
>               (Array Int String)
>               Name                    -- eof

#ifdef DEBUG

> instance Text Grammar where
>       showsPrec _ (Grammar p d t n tys e eof) = 
>               shows (getProds p) . showString "\n" .
>               shows d . showString "\n" .
>               shows t . showString "\n" .
>               shows n . showString "\n" .
>               shows tys . showString "\n" .
>               shows e . showString "\n" .
>               shows eof . showString "\n"

#endif

> startName = "%start"			-- Token 0
> eofName   = "%eof"			
> errorName = "error"			-- Token -1

> startTok   = 0
> errorTok   = (-1)
> epsilonTok = (-2)

%-----------------------------------------------------------------------------
The Mangler.

We are allowed to use the facts that the start symbol is always
non-terminal zero, and the first non-terminal in the grammar file is
non-terminal 1.

This bit is a real mess, mainly because of the error mesasge support.

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
>       nonterm' = [0..l_nt]
>       term'    = [l_nt+1..l_nt+l_t]
>       env      = (errorTok, errorName) :
>		   zip (nonterm'++term') (startName : nts ++ term)
>	l_nt     = length nts
>	l_t      = length term

>       mapToName str = 
>             case [ a | (a,r) <- env, r == str ] of
>                [a] -> Succeeded a
>                []  -> Failed ["unknown identifer `" ++ str ++ "'"]
>                _   -> Failed ["multiple use of `" ++ str ++ "'"]

> 	transRule (nt, prods, ty)
>   	  = mapToName nt				       `thenE` \nt' ->
>	    foldr (mightFails transProds) (Succeeded []) prods `thenE` \prods ->
>	    Succeeded [ (nt', lhs, code) | (lhs, code) <- prods ]

> 	transProds (lhs, code, line)
>   	  = case foldr (mightFails mapToName) (Succeeded []) lhs of
>   		Succeeded lhs' -> Succeeded (lhs', code)
>		Failed s -> Failed (map (("line " ++ show line ++ ": ") ++) s)

>	in

>	foldr (mightFails transRule) (Succeeded []) rules  `thenE` \rules' ->

>	let
>	tys   = listArray (1, l_nt) [ ty | (nm,_,ty) <- rules ]

>	env_array :: Array Int String
>	env_array = array (-1, l_nt + l_t) env

>	rules'' = mkProdInfo
>		  ((startTok, [1], "no code") : concat rules')
>		  nonterm' (errorTok : term') (head term')

>	in

>       foldr (mightFails (fixDir mapToName)) (Succeeded []) dirs
>							`thenE` \dirs' ->	

>	Succeeded (Grammar rules'' dirs' term' nonterm' tys 
>		env_array (last term' {- EOF -}))

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

> data LRAction = LR'Shift Int          -- state number
>               | LR'Reduce Int         -- rule no
>               | LR'Accept             -- :-)
>               | LR'Fail               -- :-(
>		| LR'Multiple [LRAction] LRAction	-- conflict
>       deriving(Eq

#ifdef DEBUG

>	,Text

#endif

>	)	

> type ActionTable = Array Int{-state-} (Array Int{-terminal#-} LRAction)

 instance Text LRAction where 
   showsPrec _ (LR'Shift i)    = showString ("s" ++ show i)
   showsPrec _ (LR'Reduce i) 
       = showString ("r" ++ show i)
   showsPrec _ (LR'Accept)     = showString ("acc")
   showsPrec _ (LR'Fail)       = showString (" ")
 instance Eq LRAction where { (==) = primGenericEq } 



> data Goto = Goto Int | NoGoto 
>       deriving(Eq

#ifdef DEBUG

>	,Text

#endif

>	)	

> type GotoTable = Array Int{-state-} (Array Int{-nonterminal #-} Goto)

%------------------------------------------------------------------------------

Production info is a cache of useful info about the grammar for whom
we are generating a parser for.

> data GrammarInfo = GrammarInfo
>	[Production]		-- all productions
>	(Int -> Production)	-- lookup production #
>	(Name -> [Int])		-- get (the number behind) all prod called Name
>	[Name]			-- NonTerminals
>	[Name]			-- Terminals
>	Name			-- eof terminal
>	Name			-- first terminal


> getProds          (GrammarInfo x _ _ _ _ _ _) = x
> lookupProdNo 	    (GrammarInfo _ x _ _ _ _ _) = x
> lookupProdsOfName (GrammarInfo _ _ x _ _ _ _) = x
> getNonTerminals   (GrammarInfo _ _ _ x _ _ _) = x
> getTerminals      (GrammarInfo _ _ _ _ x _ _) = x
> getEOF	    (GrammarInfo _ _ _ _ _ x _) = x
> getFirstTerm	    (GrammarInfo _ _ _ _ _ _ x) = x

> getNames :: GrammarInfo -> [Name]
> getNames          (GrammarInfo _ _ _ c d _ _) = c ++ d

> mkProdInfo :: [Production] -> [Name] -> [Name] -> Int -> GrammarInfo
> mkProdInfo prod nonterm term first_term =
>	GrammarInfo
>		prod
>		(arr !)
>		fn'
>		nonterm
>		term
>		eof
>		first_term
>     where
>	eof = last term
>	arr = listArray (0,length prod-1) prod
>	ass = combinePairs [ (a,no) | ((a,_,_),no) <- zip prod [0..] ]
>	arr' = array (0,(length ass-1)) ass
>	fn' :: Name -> [Int]
>	fn' x | x >= 0 && x < first_term = arr' ! x
>	fn' _ = error "looking up production failure"

