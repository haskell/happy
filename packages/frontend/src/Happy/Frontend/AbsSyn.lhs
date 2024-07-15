-----------------------------------------------------------------------------
Abstract syntax for grammar files.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is the abstract syntax of the language we parse.

> module Happy.Frontend.AbsSyn (
>       BookendedAbsSyn(..),
>       AbsSyn(..), Directive(..),
>       getTokenType, getTokenSpec, getParserNames, getLexer,
>       getImportedIdentity, getMonad, ErrorHandlerInfo(..), getError,
>       getPrios, getPrioNames, getExpect, getErrorHandlerExpectedList,
>       getAttributes, getAttributetype,
>       Rule(..), Prod(..), Term(..), Prec(..)
>  ) where

> import Happy.CodeGen.Common.Options (ErrorHandlerInfo(..))

> data BookendedAbsSyn
>     = BookendedAbsSyn
>         (Maybe String)       -- header
>         AbsSyn
>         (Maybe String)       -- footer

> data AbsSyn
>     = AbsSyn
>         [Directive String]   -- directives
>         [Rule]               -- productions

> data Rule
>     = Rule
>         String               -- name of the rule
>         [String]             -- parameters (see parametrized productions)
>         [Prod]               -- productions
>         (Maybe String)       -- type of the rule

> data Prod
>     = Prod
>         [Term]               -- terms that make up the rule
>         String               -- code body that runs when the rule reduces
>         Int                  -- line number
>         Prec                 -- inline precedence annotation for the rule

> data Term
>     = App
>         String               -- name of the term
>         [Term]               -- parameter arguments (usually this is empty)

> data Prec
>     = PrecNone               -- no user-specified precedence
>     | PrecShift              -- %shift
>     | PrecId String          -- %prec ID
>   deriving Show

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
>       | TokenError    String (Maybe String)   -- %error
>       | TokenErrorExpected                    -- %error.expected
>       | TokenAttributetype String             -- %attributetype
>       | TokenAttribute String String          -- %attribute
>   deriving (Eq, Show)

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
>               []  -> (False,"()","HappyIdentity","Prelude.>>=","Prelude.return")
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

> getError :: [Directive t] -> ErrorHandlerInfo
> getError ds
>       = case [ (a, mb_b) | (TokenError a mb_b) <- ds ] of
>               []                        -> DefaultErrorHandler
>               [(a,Nothing)]             -> CustomErrorHandler a
>               [(abort,Just addMessage)] -> ResumptiveErrorHandler abort addMessage
>               _   -> error "multiple error directives"

> getErrorHandlerExpectedList :: Eq t => [Directive t] -> Bool
> getErrorHandlerExpectedList ds
>       = TokenErrorExpected `elem` ds

> getAttributes :: [Directive t] -> [(String, String)]
> getAttributes ds
>         = [ (ident,typ) | (TokenAttribute ident typ) <- ds ]

> getAttributetype :: [Directive t] -> Maybe String
> getAttributetype ds
>         = case [ t | (TokenAttributetype t) <- ds ] of
>                  [t] -> Just t
>                  []  -> Nothing
>                  _   -> error "multiple attributetype directives"
