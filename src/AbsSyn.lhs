-----------------------------------------------------------------------------
$Id: AbsSyn.lhs,v 1.1.1.1 1997/02/11 13:12:06 simonm Exp $

Abstract syntax for grammar files.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is the abstract syntax of the language we parse.

> module AbsSyn 
> 	(AbsSyn(..), Directive(..),
> 	 getTokenType, getTokenSpec, getParserName, getLexer, getMonad,
>	
>	 Maybe) where

> import GenUtils 	( Maybe(..) )

> data AbsSyn
>     = AbsSyn
>         (Maybe String)					-- header
>         [Directive String]      				-- directives
>         [(String,[([String],String,Int)],Maybe String)]	-- productions
>         (Maybe String)					-- footer

#ifdef DEBUG

>   deriving (Text)

#endif

%-----------------------------------------------------------------------------
Parser Generator Directives.

ToDo: find a consistent way to analyse all the directives together and
generate some error messages.

> data Directive a
>       = TokenType   String              -- %tokentype
>       | TokenSpec  [(a,String)]         -- %token
>       | TokenName   String              -- %name
>       | TokenLexer String String        -- %lexer
>	| TokenMonad String String String -- %monad

#ifdef DEBUG

>   deriving (Text)

#endif

> getTokenType ds 
> 	= case [ t | (TokenType t) <- ds ] of 
>		[t] -> t
>		[]  -> error "no token type given"
>		_   -> error "multiple token types"

> getParserName ds
> 	= case [ t | (TokenName t) <- ds ] of
> 		[t] -> t
>		[]  -> ""
>		_   -> error "multiple parser name directives"

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

