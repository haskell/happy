-----------------------------------------------------------------------------
$Id: AbsSyn.lhs,v 1.8 2000/12/03 16:53:53 simonmar Exp $

Abstract syntax for grammar files.

(c) 1993-2000 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is the abstract syntax of the language we parse.

> module AbsSyn (
> 	AbsSyn(..), Directive(..),
> 	getTokenType, getTokenSpec, getParserNames, getLexer, getMonad,
>       getPrios, getPrioNames,
>  ) where

> data AbsSyn
>     = AbsSyn
>         (Maybe String)					-- header
>         [Directive String]      				-- directives
>         [(String,[([String],String,Int,Maybe String)],Maybe String)]	-- productions
>         (Maybe String)					-- footer

#ifdef DEBUG

>   deriving Show

#endif

%-----------------------------------------------------------------------------
Parser Generator Directives.

ToDo: find a consistent way to analyse all the directives together and
generate some error messages.

> data Directive a
>       = TokenType     String              	-- %tokentype
>       | TokenSpec     [(a,String)]         	-- %token
>       | TokenName     String (Maybe String)  	-- %name
>       | TokenLexer    String String        	-- %lexer
>	| TokenMonad    String String String 	-- %monad
>	| TokenNonassoc [String]	  	-- %nonassoc
>	| TokenRight    [String]		-- %right
>	| TokenLeft     [String]		-- %left

#ifdef DEBUG

>   deriving Show

#endif

> getTokenType ds 
> 	= case [ t | (TokenType t) <- ds ] of 
>		[t] -> t
>		[]  -> error "no token type given"
>		_   -> error "multiple token types"

> getParserNames ds = [ t | t@(TokenName _ _) <- ds ]

> getLexer ds 
> 	= case [ (a,b) | (TokenLexer a b) <- ds ] of
> 		[t] -> Just t
>		[]  -> Nothing
>		_   -> error "multiple lexer directives"

> getMonad ds 
> 	= case [ (a,b,c) | (TokenMonad a b c) <- ds ] of
> 		[t] -> Just t
>		[]  -> Nothing
>		_   -> error "multiple monad directives"

> getTokenSpec ds = concat [ t | (TokenSpec t) <- ds ]

> getPrios ds = [ d | d <- ds,
>                 case d of
>		    TokenNonassoc _ -> True
>		    TokenLeft _ -> True
>		    TokenRight _ -> True
>		    _ -> False
>               ]

> getPrioNames (TokenNonassoc s) = s
> getPrioNames (TokenLeft s)     = s
> getPrioNames (TokenRight s)    = s
