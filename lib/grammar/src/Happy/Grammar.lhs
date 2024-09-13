/-----------------------------------------------------------------------------
The Grammar data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> -- | This module exports the 'Grammar' data type, which
> module Happy.Grammar (
>       Name,
>
>       Production(..), Grammar(..),
>       Priority(..),
>       Assoc(..),
>       Pragmas(..), ErrorHandlerType(..),
>
>       errorName, errorTok, startName, dummyName, firstStartTok, dummyTok,
>       eofName, epsilonTok,
>
>       mapDollarDollar
>       ) where

> import Data.Array
> import Data.Char (isAlphaNum)
> type Name = Int

> data Production
>       = Production Name [Name] (String,[Int]) Priority
>       deriving Show

> data Grammar
>       = Grammar {
>               productions       :: [Production],
>               lookupProdNo      :: Int -> Production,
>               lookupProdsOfName :: Name -> [Int],
>               token_specs       :: [(Name,String)],
>               terminals         :: [Name],
>               non_terminals     :: [Name],
>               starts            :: [(String,Name,Name,Bool)],
>               types             :: Array Int (Maybe String),
>               token_names       :: Array Int String,
>               first_nonterm     :: Name,
>               first_term        :: Name,
>               eof_term          :: Name,
>               priorities        :: [(Name,Priority)],
>               attributes        :: [(String,String)],
>               attributetype     :: String
>       }

> instance Show Grammar where
>       showsPrec _ (Grammar
>               { productions           = p
>               , token_specs           = t
>               , terminals             = ts
>               , non_terminals         = nts
>               , starts                = sts
>               , types                 = tys
>               , token_names           = e
>               , first_nonterm         = fnt
>               , first_term            = ft
>               , eof_term              = eof
>               })
>        = showString "productions = "     . shows p
>        . showString "\ntoken_specs = "   . shows t
>        . showString "\nterminals = "     . shows ts
>        . showString "\nnonterminals = "  . shows nts
>        . showString "\nstarts = "        . shows sts
>        . showString "\ntypes = "         . shows tys
>        . showString "\ntoken_names = "   . shows e
>        . showString "\nfirst_nonterm = " . shows fnt
>        . showString "\nfirst_term = "    . shows ft
>        . showString "\neof = "           . shows eof
>        . showString "\n"

> data Assoc = LeftAssoc | RightAssoc | None
>       deriving Show

> data Priority = No | Prio Assoc Int | PrioLowest
>       deriving Show

> instance Eq Priority where
>   No == No = True
>   Prio _ i == Prio _ j = i == j
>   _ == _ = False

> data ErrorHandlerType
>   = ErrorHandlerTypeDefault
>   | ErrorHandlerTypeExpList

> -- | Stuff like `\%monad`, `\%expect`
> data Pragmas
>       = Pragmas {
>               token_type        :: String,
>               imported_identity :: Bool,
>               monad             :: (Bool,String,String,String,String),
>               expect            :: Maybe Int,
>               lexer             :: Maybe (String,String),
>               error_handler     :: Maybe String,
>               error_sig         :: ErrorHandlerType
>       }

-----------------------------------------------------------------------------
-- Magic name values

All the tokens in the grammar are mapped onto integers, for speed.
The namespace is broken up as follows:

epsilon         = 0
error           = 1
dummy           = 2
%start          = 3..s
non-terminals   = s..n
terminals       = n..m
%eof            = m

where n_nonterminals = n - 3 (including %starts)
      n_terminals    = 1{-error-} + (m-n) + 1{-eof-} (including error and %eof)

These numbers are deeply magical, change at your own risk.  Several
other places rely on these being arranged as they are, including
ProduceCode.lhs and the various HappyTemplates.

Unfortunately this means you can't tell whether a given token is a
terminal or non-terminal without knowing the boundaries of the
namespace, which are kept in the Grammar structure.

In hindsight, this was probably a bad idea.

In normal and GHC-based parsers, these numbers are also used in the
generated grammar itself, except that the error token is mapped to -1.
For array-based parsers, see the note in Tabular/LALR.lhs.

> startName, eofName, errorName, dummyName :: String
> startName = "%start" -- with a suffix, like %start_1, %start_2 etc.
> eofName   = "%eof"
> errorName = "error"
> dummyName = "%dummy"  -- shouldn't occur in the grammar anywhere

> firstStartTok, dummyTok, errorTok, epsilonTok :: Name
> firstStartTok   = 3
> dummyTok        = 2
> errorTok        = 1
> epsilonTok      = 0

-----------------------------------------------------------------------------
Replace $$ with an arbitrary string, being careful to avoid ".." and '.'.

> mapDollarDollar :: String -> Maybe (String -> String)
> mapDollarDollar code0 = go code0 ""
>   where go code acc =
>           case code of
>               [] -> Nothing
>
>               '"'  :r    -> case reads code :: [(String,String)] of
>                                []       -> go r ('"':acc)
>                                (s,r'):_ -> go r' (reverse (show s) ++ acc)
>               a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc)
>               '\'' :r    -> case reads code :: [(Char,String)] of
>                                []       -> go r ('\'':acc)
>                                (c,r'):_ -> go r' (reverse (show c) ++ acc)
>               '\\':'$':r -> go r ('$':acc)
>               '$':'$':r  -> Just (\repl -> reverse acc ++ repl ++ r)
>               c:r  -> go r (c:acc)
