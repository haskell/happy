-----------------------------------------------------------------------------
Abstract syntax for grammar files.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is the abstract syntax of the language we parse.

> module AbsSyn (
>       AbsSyn(..), Directive(..),
>       getTokenType, getTokenSpec, getParserNames, getLexer,
>       getImportedIdentity, getMonad, getError,
>       getPrios, getPrioNames, getExpect,
>       getAttributes, getAttributetype,
>       Rule,Prod,Term(..)
>  ) where

> data AbsSyn
>     = AbsSyn
>         (Maybe String)                                        -- header
>         [Directive String]                                    -- directives
>         [Rule]        -- productions
>         (Maybe String)                                        -- footer

> type Rule     = (String,[String],[Prod],Maybe String)
> type Prod     = ([Term],String,Int,Maybe String)
> data Term     = App String [Term]



#ifdef DEBUG

>   deriving Show

#endif

%-----------------------------------------------------------------------------
Parser Generator Directives.

ToDo: find a consistent way to analyse all the directives together and
generate some error messages.

> data Directive a
>       = TokenType     String                  -- %tokentype
>       | TokenSpec     [(a,String)]            -- %token
>       | TokenName     String (Maybe String) Bool -- %name/%partial (True <=> %partial)
>       | TokenLexer    String String           -- %lexer
>       | TokenImportedIdentity                                 -- %importedidentity
>       | TokenMonad    String String String String -- %monad
>       | TokenNonassoc [String]                -- %nonassoc
>       | TokenRight    [String]                -- %right
>       | TokenLeft     [String]                -- %left
>       | TokenExpect   Int                     -- %expect
>       | TokenError    String                  -- %error
>       | TokenAttributetype String             -- %attributetype
>       | TokenAttribute String String          -- %attribute


#ifdef DEBUG

>   deriving Show

#endif

> getTokenType :: [Directive t] -> String
> getTokenType ds
>       = case [ t | (TokenType t) <- ds ] of
>               [t] -> t
>               []  -> error "no token type given"
>               _   -> error "multiple token types"

> getParserNames :: [Directive t] -> [Directive t]
> getParserNames ds = [ t | t@(TokenName _ _ _) <- ds ]

> getLexer :: [Directive t] -> Maybe (String, String)
> getLexer ds
>       = case [ (a,b) | (TokenLexer a b) <- ds ] of
>               [t] -> Just t
>               []  -> Nothing
>               _   -> error "multiple lexer directives"

> getImportedIdentity :: [Directive t] -> Bool
> getImportedIdentity ds
>       = case [ (()) | TokenImportedIdentity <- ds ] of
>               [_] -> True
>               []  -> False
>               _   -> error "multiple importedidentity directives"

> getMonad :: [Directive t] -> (Bool, String, String, String, String)
> getMonad ds
>       = case [ (True,a,b,c,d) | (TokenMonad a b c d) <- ds ] of
>               [t] -> t
>               []  -> (False,"()","HappyIdentity",">>=","return")
>               _   -> error "multiple monad directives"

> getTokenSpec :: [Directive t] -> [(t, String)]
> getTokenSpec ds = concat [ t | (TokenSpec t) <- ds ]

> getPrios :: [Directive t] -> [Directive t]
> getPrios ds = [ d | d <- ds,
>                 case d of
>                   TokenNonassoc _ -> True
>                   TokenLeft _ -> True
>                   TokenRight _ -> True
>                   _ -> False
>               ]

> getPrioNames :: Directive t -> [String]
> getPrioNames (TokenNonassoc s) = s
> getPrioNames (TokenLeft s)     = s
> getPrioNames (TokenRight s)    = s
> getPrioNames _                 = error "Not an associativity token"

> getExpect :: [Directive t] -> Maybe Int
> getExpect ds
>         = case [ n | (TokenExpect n) <- ds ] of
>                 [t] -> Just t
>                 []  -> Nothing
>                 _   -> error "multiple expect directives"

> getError :: [Directive t] -> Maybe String
> getError ds
>       = case [ a | (TokenError a) <- ds ] of
>               [t] -> Just t
>               []  -> Nothing
>               _   -> error "multiple error directives"

> getAttributes :: [Directive t] -> [(String, String)]
> getAttributes ds
>         = [ (ident,typ) | (TokenAttribute ident typ) <- ds ]

> getAttributetype :: [Directive t] -> Maybe String
> getAttributetype ds
>         = case [ t | (TokenAttributetype t) <- ds ] of
>                  [t] -> Just t
>                  []  -> Nothing
>                  _   -> error "multiple attributetype directives"
