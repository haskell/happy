%-----------------------------------------------------------------------------
AbsSyn.lhs
(c) 1993 Andy Gill, Simon Marlow
%-----------------------------------------------------------------------------

Here is the abstract syntax of the language we parse.

> module AbsSyn 
> 	(AbsSyn(..), Directive(..),
> 	 getTokenType, getTokenSpec, getParserName, getNewline,
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

#ifdef GOFER

> instance Text AbsSyn where { showsPrec = primPrint } 

#endif

%-----------------------------------------------------------------------------
Parser Generator Directives.

ToDo: find a consistent way to analyse all the directives together and
generate some error messages.

> data Directive a
>       = TokenType   String            -- %tokentype
>       | TokenSpec  [(a,String)]       -- %token
>       | TokenName   String            -- %name
>       | TokenNewline String           -- %newline

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

> getNewline ds 
> 	= case [ t | (TokenNewline t) <- ds ] of
> 		[t] -> Just t
>		[]  -> Nothing
>		_   -> error "multiple newline directives"

> getTokenSpec ds = concat [ t | (TokenSpec t) <- ds ]

#ifdef GOFER

> instance Text (Directive a) where { showsPrec = primPrint } 

#endif
