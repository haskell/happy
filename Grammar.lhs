%-----------------------------------------------------------------------------
Grammar.lhs
(c) Andy Gill, Simon Marlow 1993
%-----------------------------------------------------------------------------
Here is our mid-section datatype

> module Grammar (
> 	Name(..), 
> 	isEmpty, isNT, isT, nameToMaybeInt, nameToInt, 
>	
>	Production(..),  Productions(..), Terminals(..), NonTerminals(..),
>	Grammar(..), startName, eofName, mangler, fixDir, getTerm,
>	checkRules, 
>	
>	LRAction(..), ActionTable(..), Goto(..), GotoTable(..),
>	
>	GrammarInfo(..),
>	getProds, lookupProdNo, lookupProdsOfName, getNonTerminals,
>	getTerminals, getEOF, getNames, mkProdInfo ) where

> import GenUtils
> import AbsSyn

Change this into the most useful, depending on what
you want Simon.

> data Name
>       = Terminal    Int
>       | NonTerminal Int
>       | Epsilon
>  deriving(Eq,Ord)

#ifdef __GOFER__

> instance Eq Name where { (==) = primGenericEq } 
> instance Ord Name where { (<=) = primGenericLe } 

#endif 

> isEmpty Epsilon = True
> isEmpty _       = False

> isNT (NonTerminal _) = True
> isNT _               = False

> isT (Terminal _) = True
> isT _            = False

> nameToMaybeInt :: Name -> Maybe Int
> nameToMaybeInt (Terminal n)    = Just n
> nameToMaybeInt (NonTerminal n) = Just n
> nameToMaybeInt _	         = Nothing

> nameToInt :: Name -> Int
> nameToInt (Terminal n)    = n
> nameToInt (NonTerminal n) = n
> nameToInt _ = error "nameToInt"

> instance Text Name where
>       showsPrec _ (Terminal i)    = showString "T." . shows i 
>       showsPrec _ (NonTerminal i) = showString "NT." . shows i
>       showsPrec _ Epsilon         = showString "e"

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

> startName = "%start"
> eofName   = "%eof"

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

>	checkRules ([n | (n,_,_) <- rules]) "" []	   `thenE` \nts   ->

>	let
>       term     = concat (map getTerm dirs) ++ [eofName]
>       nonterm  = [startName] ++ nts
>       nonterm' = take l_nt (map NonTerminal  [0::Int..])
>       term'    = take l_t  (map Terminal     [l_nt..]  )
>       env      = zip (nonterm'++term') (nonterm ++ term)
>	l_nt     = length nonterm
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
>	tys   = listArray (1, l_nt-1) [ ty | (nm,_,ty) <- rules ]

>	env_array :: Array Int String
>	env_array = array (0, l_nt + l_t - 1) 
>		(zipWith (:=) [0..] (nonterm ++ term))

>	rules'' = mkProdInfo
>		  ((NonTerminal 0, [NonTerminal 1], "no code") : concat rules')
>		  nonterm' term'

>	in

>       foldr (mightFails (fixDir mapToName)) (Succeeded []) dirs
>							`thenE` \dirs' ->	

>	Succeeded (Grammar rules'' dirs' term' nonterm' tys 
>		env_array (last term' {- EOF -}))
>  

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
> fixDir f (TokenNewline str) = Succeeded (TokenNewline str)
> fixDir f (TokenSpec stuff)  
> 	= foldr (mightFails (\(a,b) -> f a `thenE` \a -> Succeeded (a,b))) 
>			(Succeeded []) stuff `thenE` \stuff' ->
>	  Succeeded (TokenSpec stuff')

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

#ifdef __GOFER__

> instance Text LRAction where 
>   showsPrec _ (LR'Shift i)    = showString ("s" ++ show i)
>   showsPrec _ (LR'Reduce i) 
>       = showString ("r" ++ show i)
>   showsPrec _ (LR'Accept)     = showString ("acc")
>   showsPrec _ (LR'Fail)       = showString (" ")
> instance Eq LRAction where { (==) = primGenericEq } 

#endif

> data Goto = Goto Int | NoGoto 
>       deriving(Eq

#ifdef DEBUG

>	,Text

#endif

>	)	

#ifdef __GOFER__

> instance Text Goto where { showsPrec = primPrint } 
> instance Eq Goto where { (==) = primGenericEq } 

#endif

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

> getProds          (GrammarInfo a _ _ _ _ _) = a
> lookupProdNo 	    (GrammarInfo _ b _ _ _ _) = b
> lookupProdsOfName (GrammarInfo _ _ c _ _ _) = c
> getNonTerminals   (GrammarInfo _ _ _ c _ _) = c
> getTerminals      (GrammarInfo _ _ _ _ c _) = c
> getEOF	    (GrammarInfo _ _ _ _ _ c) = c

> getNames :: GrammarInfo -> [Name]
> getNames          (GrammarInfo _ _ _ c d _) = c ++ d

> mkProdInfo :: [Production] -> [Name] -> [Name] -> GrammarInfo
> mkProdInfo prod nonterm term =
>	GrammarInfo
>		prod
>		(arr !)
>		fn'
>		nonterm
>		term
>		eof
>     where
>	eof = last term
>	arr = listArray (0,length prod-1) prod
>	ass = combinePairs [ (a,no) | ((a,_,_),no) <- zip prod [0::Int..] ]
>	arr' = array (0,(length ass-1))
>		[ nameToInt n := num | (n,num) <- ass ]
>	fn' :: Name -> [Int]
>	fn' x | isNT x = arr' ! (nameToInt x)
>	fn' _ = error "looking up production failure"

