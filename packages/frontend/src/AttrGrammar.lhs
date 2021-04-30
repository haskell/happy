> module AttrGrammar
> ( AgToken (..)
> , AgRule (..)
> , HasLexer (..)
> , agLexAll
> , subRefVal
> , selfRefVal
> , rightRefVal
> ) where

> import Data.Char
> import ParseMonad.Class

> data AgToken
>   = AgTok_LBrace
>   | AgTok_RBrace
>   | AgTok_Where
>   | AgTok_Semicolon
>   | AgTok_Eq
>   | AgTok_SelfRef String
>   | AgTok_SubRef (Int, String)
>   | AgTok_RightmostRef String
>   | AgTok_Unknown String
>   | AgTok_EOF
>  deriving (Show,Eq,Ord)

> subRefVal :: AgToken -> (Int, String)
> subRefVal   (AgTok_SubRef x)       = x
> subRefVal   _ = error "subRefVal: Bad value"
> selfRefVal :: AgToken -> String
> selfRefVal  (AgTok_SelfRef x)      = x
> selfRefVal  _ = error "selfRefVal: Bad value"
> rightRefVal :: AgToken -> String
> rightRefVal (AgTok_RightmostRef x) = x
> rightRefVal _ = error "rightRefVal: Bad value"

> data AgRule
>   = SelfAssign String [AgToken]
>   | SubAssign (Int,String) [AgToken]
>   | RightmostAssign String [AgToken]
>   | Conditional [AgToken]
>  deriving (Show,Eq,Ord)

-----------------------------------------------------------------
-- For the most part, the body of the attribute grammar rules
-- is uninterpreted haskell expressions.  We only need to know about
--    a) braces and semicolons to break the rules apart
--    b) the equals sign to break the rules into LValues and the RHS
--    c) attribute references, which are $$, $x (postivie integer x)
--       or $> (for the rightmost symbol) followed by an optional
--       attribute specifier, which is a dot followed by a
--       Haskell variable identifier
--         Examples:
--            $$
--            $1
--            $>
--            $$.pos
--            $3.value
--            $2.someAttribute0'
--
-- Everything else can be treated as uninterpreted strings.  Our munging
-- will wreck column alignment so attribute grammar specifications must
-- not rely on layout.

> agLexAll :: String -> Int -> ParseResult [AgToken]
> agLexAll = aux []
>  where aux toks [] _ = Right (reverse toks)
>        aux toks s l  = agLexer (\t -> aux (t:toks)) s l

> instance HasLexer AgToken where
>   lexToken = agLexer

> agLexer :: (AgToken -> Pfunc a) -> Pfunc a
> agLexer cont []         = cont AgTok_EOF []
> agLexer cont ('{':rest) = cont AgTok_LBrace rest
> agLexer cont ('}':rest) = cont AgTok_RBrace rest
> agLexer cont (';':rest) = cont AgTok_Semicolon rest
> agLexer cont ('=':rest) = cont AgTok_Eq rest
> agLexer cont ('w':'h':'e':'r':'e':rest) = cont AgTok_Where rest
> agLexer cont ('$':'$':rest) = agLexAttribute cont (\a -> AgTok_SelfRef a) rest
> agLexer cont ('$':'>':rest) = agLexAttribute cont (\a -> AgTok_RightmostRef a) rest
> agLexer cont s@('$':rest) =
>     let (n,rest') = span isDigit rest
>     in if null n
>           then agLexUnknown cont s
>           else agLexAttribute cont (\a -> AgTok_SubRef (read n,a)) rest'
> agLexer cont s@(c:rest)
>     | isSpace c = agLexer cont (dropWhile isSpace rest)
>     | otherwise = agLexUnknown cont s

> agLexUnknown :: (AgToken -> Pfunc a) -> Pfunc a
> agLexUnknown cont s = let (u,rest) = aux [] s in cont (AgTok_Unknown u) rest
>   where aux t [] = (reverse t,[])
>         aux t ('$':c:cs)
>            | c /= '$' && not (isDigit c)  = aux ('$':t) (c:cs)
>            | otherwise                    = (reverse t,'$':c:cs)
>         aux t (c:cs)
>            | isSpace c || c `elem` "{};=" = (reverse t,c:cs)
>            | otherwise                    = aux (c:t) cs

> agLexAttribute :: (AgToken -> Pfunc a) -> (String -> AgToken) -> Pfunc a
> agLexAttribute cont k ('.':x:xs)
>        | isLower x = let (ident,rest) = span (\c -> isAlphaNum c || c == '\'') xs in cont (k (x:ident)) rest
>        | otherwise = \_ -> Left "bad attribute identifier"
> agLexAttribute cont k rest = cont (k "") rest
