-----------------------------------------------------------------------------
$Id: Lexer.lhs,v 1.19 2001/04/27 10:10:23 simonmar Exp $

The lexer.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 400
#define isAlphaNum isAlphanum
#endif

> module Lexer (
>       Token(..),
>       TokenId(..),
>       lexer ) where

> import ParseMonad        

> import Char ( isSpace, isAlphaNum )

> data Token 
>       = TokenInfo String TokenId
>       | TokenKW          TokenId
>	| TokenEOF

> tokenToId :: Token -> TokenId
> tokenToId (TokenInfo _ i) = i
> tokenToId (TokenKW i) = i

> instance Eq Token where
>       i == i' = tokenToId i == tokenToId i'

> instance Ord Token where
>       i <= i' = tokenToId i <= tokenToId i'

> data TokenId 
>       = TokId                 -- words and symbols
>       | TokSpecId_TokenType   -- %tokentype
>       | TokSpecId_Token       -- %token
>       | TokSpecId_Name        -- %name
>       | TokSpecId_Lexer       -- %lexer
>       | TokSpecId_Monad	-- %monad
>       | TokSpecId_Nonassoc    -- %nonassoc
>       | TokSpecId_Left        -- %left
>       | TokSpecId_Right       -- %right
>       | TokSpecId_Prec        -- %prec
>       | TokCodeQuote          -- stuff inside { .. }
>       | TokColon              -- :
>       | TokSemiColon          -- ;
>       | TokDoubleColon        -- ::
>       | TokDoublePercent      -- %%
>       | TokBar                -- |
>       deriving (Eq,Ord

#ifdef DEBUG

>       	,Show

#endif

>		)

ToDo: proper text instance here, for use in parser error messages.

> lexer :: (Token -> P a) -> P a
> lexer cont "" = cont TokenEOF ""
> lexer cont ('-':'-':r) = lexer cont (dropWhile (/= '\n') r)
> lexer cont ('{':'-':r) = \line -> lexNestedComment line 
>				(\r -> lexer cont r) r line
> lexer cont (c:rest) = nextLex cont c rest

> nextLex :: (Token -> P a) -> Char -> P a
> nextLex cont c = case c of
>  	'\n' 	-> \rest line -> lexer cont rest (line+1)
>  	'%' 	-> lexPercent cont
>  	':' 	-> lexColon cont
>  	';' 	-> cont (TokenKW TokSemiColon)

>  	'|' 	-> cont (TokenKW TokBar)
>  	'\''	-> lexChar cont
>  	'"'{-"-}-> lexString cont
>  	'{' 	-> lexCode cont
>  	c 	
>	  | isSpace c -> lexer cont
>	  |  c >= 'a' && c <= 'z' 
>	     || c >= 'A' && c <= 'Z' -> lexId cont c
>	c       -> lexError ("lexical error before `" ++ c : "'")

Percents come in two forms, in pairs, or 
followed by a special identifier.

> lexPercent cont s = case s of
> 	'%':rest -> cont (TokenKW TokDoublePercent) rest
> 	't':'o':'k':'e':'n':'t':'y':'p':'e':rest -> 
>		cont (TokenKW TokSpecId_TokenType) rest
> 	't':'o':'k':'e':'n':rest ->
> 		cont (TokenKW TokSpecId_Token) rest
> 	'n':'a':'m':'e':rest ->
> 		cont (TokenKW TokSpecId_Name) rest
> 	'm':'o':'n':'a':'d':rest ->
> 		cont (TokenKW TokSpecId_Monad) rest
> 	'l':'e':'x':'e':'r':rest ->
> 		cont (TokenKW TokSpecId_Lexer) rest
>       'n':'o':'n':'a':'s':'s':'o':'c':rest ->
>               cont (TokenKW TokSpecId_Nonassoc) rest
>       'l':'e':'f':'t':rest ->
>               cont (TokenKW TokSpecId_Left) rest
>       'r':'i':'g':'h':'t':rest ->
>               cont (TokenKW TokSpecId_Right) rest
>       'p':'r':'e':'c':rest ->
>               cont (TokenKW TokSpecId_Prec) rest
>	_ -> lexError ("unrecognised directive: %" ++ 
>				takeWhile (not.isSpace) s) s

> lexColon cont (':':rest) = cont (TokenKW TokDoubleColon) rest
> lexColon cont rest       = cont (TokenKW TokColon) rest

> lexId cont c rest = 
>	readId rest (\ id rest' -> cont (TokenInfo (c:id) TokId) rest')

> lexChar cont rest = lexReadChar rest 
>	(\ id -> cont (TokenInfo ("'" ++ id ++ "'") TokId))

> lexString cont rest = lexReadString rest 
>	(\ id -> cont (TokenInfo ("\"" ++ id ++ "\"") TokId))

> lexCode cont rest = lexReadCode rest 0 "" cont

> cleanupCode s = 
>    dropWhile isSpace (reverse (dropWhile isSpace (reverse s)))

This has to match for @}@ that are {\em not} in strings.  The code
here is a bit tricky, but should work in most cases.

> lexReadCode s n c = case s of
>	'\n':r -> \cont l ->  lexReadCode r n ('\n':c) cont (l+1)
>
> 	'{' :r -> lexReadCode r (n+1) ('{':c)
>
> 	'}' :r
>		| n == 0    -> \cont -> cont (TokenInfo (
>				cleanupCode (reverse c)) TokCodeQuote) r
>		| otherwise -> lexReadCode r (n-1) ('}':c)
>
> 	'"'{-"-}:r -> lexReadString r (\ str r' -> 
>         	      lexReadCode r' n ('"' : (reverse str) ++ '"' : c))
>
> 	a: '\'':r | isAlphaNum a -> lexReadCode r n ('\'':a:c)
>
> 	'\'' :r	-> lexReadChar r (\ str r' -> 
>         	   lexReadCode r' n ('\'' : (reverse str) ++ '\'' : c))
>
> 	ch:r -> lexReadCode r n (ch:c)
>
> 	[] -> \cont -> lexError "No closing '}' in code segment" []

----------------------------------------------------------------------------
Utilities that read the rest of a token.

> readId :: String -> (String -> String -> a) -> a
> readId (c:r) fn | isIdPart c = readId r (fn . (:) c)
> readId r     fn = fn [] r

> isIdPart :: Char -> Bool
> isIdPart c =
>	   c >= 'a' && c <= 'z' 
>	|| c >= 'A' && c <= 'Z' 
>	|| c >= '0' && c <= '9' 
>	|| c == '_'

> lexReadChar :: String -> (String -> String -> a) -> a
> lexReadChar ('\'':r)      fn = fn "" r
> lexReadChar ('\\':'\'':r) fn = lexReadChar r (fn . (:) '\\' . (:) '\'')
> lexReadChar ('\\':c:r)    fn = lexReadChar r (fn . (:) '\\' . (:) c)
> lexReadChar (c:r)         fn = lexReadChar r (fn . (:) c)
> lexReadChar []            fn = fn "" []

> lexReadString :: String -> (String -> String -> a) -> a
> lexReadString ('"'{-"-}:r) fn = fn "" r
> lexReadString ('\\':'"':r) fn = lexReadString r (fn . (:) '\\' . (:) '"')
> lexReadString ('\\':c:r)   fn = lexReadString r (fn . (:) '\\' . (:) c)
> lexReadString (c:r)        fn = lexReadString r (fn . (:) c)
> lexReadString []           fn = fn "" []

> lexError err s l = failP (show l ++ ": " ++ err ++ "\n") s l

> lexNestedComment l cont r = 
>   case r of
>	'-':'}':r -> cont r
>	'{':'-':r -> \line -> lexNestedComment line 
>			(\r -> lexNestedComment l cont r) r line
>	'\n':r    -> \line -> lexNestedComment l cont r (line+1)
>	c:r       -> lexNestedComment l cont r
>	""	  -> \_ -> lexError "unterminated comment" r l
