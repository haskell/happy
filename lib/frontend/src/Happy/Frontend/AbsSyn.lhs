-----------------------------------------------------------------------------
Abstract syntax for grammar files.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

Here is the abstract syntax of the language we parse.

> module Happy.Frontend.AbsSyn (
>       BookendedAbsSyn(..),
>       AbsSyn(..), Directive(..),
>       getTokenType, getTokenSpec, getParserNames, getLexer,
>       getImportedIdentity, getMonad, getError,
>       getPrios, getPrioNames, getExpect, getErrorHandlerType,
>       getAttributes, getAttributetype, getAttributeGrammarExtras,
>       parseTokenSpec,
>       Rule(..), Prod(..), Term(..), Prec(..),
>       TokenSpec(..) -- reexport
>  ) where

> import Data.Char (isAlphaNum)
> import Happy.Grammar
>   ( ErrorHandlerType(..)
>   , TokenSpec(..)
>   , AttributeGrammarExtras(..)
>   )
> import Happy.Grammar.ExpressionWithHole

> data BookendedAbsSyn
>     = BookendedAbsSyn
>         (Maybe String)       -- header
>         (AbsSyn String)
>         (Maybe String)       -- footer

> data AbsSyn e
>     = AbsSyn
>         [Directive String]   -- directives
>         [Rule e]             -- productions

> data Rule e
>     = Rule
>         String               -- name of the rule
>         [String]             -- parameters (see parametrized productions)
>         [Prod e]             -- productions
>         (Maybe String)       -- type of the rule

> data Prod e
>     = Prod
>         [Term]               -- terms that make up the rule
>         e                    -- code body that runs when the rule reduces
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

>
> data Directive a
>       = TokenType     String                  -- %tokentype
>       | TokenSpec     [(a, TokenSpec)]        -- %token
>       | TokenName     String (Maybe String) Bool -- %name/%partial (True <=> %partial)
>       | TokenLexer    String String           -- %lexer
>       | TokenErrorHandlerType String          -- %errorhandlertype
>       | TokenImportedIdentity                                 -- %importedidentity
>       | TokenMonad    String String String String -- %monad
>       | TokenNonassoc [String]                -- %nonassoc
>       | TokenRight    [String]                -- %right
>       | TokenLeft     [String]                -- %left
>       | TokenExpect   Int                     -- %expect
>       | TokenError    String                  -- %error
>       | TokenAttributetype String             -- %attributetype
>       | TokenAttribute String String          -- %attribute
>   deriving Show

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

> getTokenSpec :: [Directive t] -> [(t, TokenSpec)]
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

> getErrorHandlerType :: [Directive t] -> ErrorHandlerType
> getErrorHandlerType ds
>       = case [ a | (TokenErrorHandlerType a) <- ds ] of
>               [t] -> case t of
>                        "explist" -> ErrorHandlerTypeExpList
>                        "default" -> ErrorHandlerTypeDefault
>                        _ -> error "unsupported %errorhandlertype value"
>               []  -> ErrorHandlerTypeDefault
>               _   -> error "multiple errorhandlertype directives"

> getAttributes :: [Directive t] -> [(String, String)]
> getAttributes ds
>         = [ (ident,typ) | (TokenAttribute ident typ) <- ds ]

> getAttributetype :: [Directive t] -> Maybe String
> getAttributetype ds
>         = case [ t | (TokenAttributetype t) <- ds ] of
>                  [t] -> Just t
>                  []  -> Nothing
>                  _   -> error "multiple attributetype directives"

> getAttributeGrammarExtras :: [Directive t] -> Maybe AttributeGrammarExtras
> getAttributeGrammarExtras ds = case (getAttributes ds, getAttributetype ds) of
>   ([], Nothing) -> Nothing
>   (as, Just at) -> Just $ AttributeGrammarExtras {
>           attributes = as,
>           attributetype = at
>       }
>   (_ : _, Nothing) -> error "attributes found without attribute type directive"

> -- | Parse a token spec.
> --
> -- The first occurence of '$$' indicates an expression in which the '$$'
> -- will be substituted for the actual lexed token. '$$' in string or char
> -- literals ('".."' and '\'.\'') however does not count.
> parseTokenSpec :: String -> TokenSpec
> parseTokenSpec code0 = go code0 ""
>   where go code acc =
>           case code of
>               [] -> TokenFixed code0
>
>               '"'  :r    -> case reads code :: [(String,String)] of
>                                []       -> go r ('"':acc)
>                                (s,r'):_ -> go r' (reverse (show s) ++ acc)
>               a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc)
>               '\'' :r    -> case reads code :: [(Char,String)] of
>                                []       -> go r ('\'':acc)
>                                (c,r'):_ -> go r' (reverse (show c) ++ acc)
>               '\\':'$':r -> go r ('$':acc)
>               '$':'$':r  -> TokenWithValue $ ExpressionWithHole (reverse acc) r
>               c:r  -> go r (c:acc)
