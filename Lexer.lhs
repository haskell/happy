%-----------------------------------------------------------------------------
Lexer.lhs
(c) Andy Gill, Simon Marlow 1993
%-----------------------------------------------------------------------------

> module Lexer (
>       Token(..),
>       TokenId(..),
>       ourLexer ) where
        
> import GenUtils

> data Token 
>       = TokenInfo String TokenId
>       | TokenKW          TokenId

#ifdef DEBUG

>       deriving (Text) 

#endif

> tokenToId :: Token -> TokenId
> tokenToId (TokenInfo _ i) = i
> tokenToId (TokenKW i) = i

> instance Eq Token where
>       i == i' = tokenToId i == tokenToId i'

> instance Ord Token where
>       i <= i' = tokenToId i <= tokenToId i'

#ifdef GOFER

> instance Text Token where { showsPrec = primPrint } 

#endif

> data TokenId 
>       = TokId                 -- words and symbols
>       | TokSpecId_TokenType   -- %tokentype
>       | TokSpecId_Token       -- %token
>       | TokSpecId_Name        -- %name
>       | TokSpecId_Newline     -- %newline
>       | TokCodeQuote          -- stuff inside { .. }
>       | TokColon              -- :
>       | TokSemiColon          -- ;
>       | TokDoubleColon        -- ::
>       | TokDoublePercent      -- %%
>       | TokBar                -- |
>       | TokNewLine            -- the newline
>       deriving (Eq,Ord

#ifdef DEBUG

>       	,Text

#endif

>		)

ToDo: proper text instance here, for use in parser error messages.

#ifdef GOFER

> instance Text TokenId where { showsPrec = primPrint } 
> instance Eq TokenId where { (==) = primGenericEq } 
> instance Ord TokenId where { (<=) = primGenericLe } 

#endif

> ourLexer :: String -> [Token]
> ourLexer [] = []
> ourLexer (c:rest) = nextLex c rest

> nextLex :: Char -> String -> [Token]
> nextLex c = case c of
>  	' '  	-> ourLexer
>  	'\t' 	-> ourLexer
>  	'\n' 	-> \r -> TokenKW TokNewLine : ourLexer r
>  	'%' 	-> lexPercent
>  	':' 	-> lexColon
>  	';' 	-> \r -> TokenKW TokSemiColon : ourLexer r

>  	'|' 	-> \r -> TokenKW TokBar       : ourLexer r
>  	'\''	-> lexChar
>  	'"' 	-> lexString
>  	'{' 	-> lexCode
>  	c 	|  c >= 'a' && c <= 'z' 
>		|| c >= 'A' && c <= 'Z' -> lexId c
>  	_ 	-> \y -> error ("lexical error before `" ++ c : "'")

Percents come in two forms, in pairs, or 
followed by a special identifier.

ToDo: error messages for LexPercent.

> lexPercent :: String -> [Token]
> lexPercent ('%':rest) = TokenKW TokDoublePercent : ourLexer rest
> lexPercent ('t':'o':'k':'e':'n':'t':'y':'p':'e':rest) = TokenKW TokSpecId_TokenType : ourLexer rest
> lexPercent ('t':'o':'k':'e':'n':rest) = TokenKW TokSpecId_Token : ourLexer rest
> lexPercent ('n':'a':'m':'e':rest) = TokenKW TokSpecId_Name : ourLexer rest
> lexPercent ('n':'e':'w':'l':'i':'n':'e':rest) = TokenKW TokSpecId_Newline : ourLexer rest

> lexColon :: String -> [Token]
> lexColon (':':rest) = TokenKW TokDoubleColon : ourLexer rest
> lexColon rest       = TokenKW TokColon       : ourLexer rest

> lexId :: Char -> String -> [Token]
> lexId c rest = readId rest (\ id rest' -> TokenInfo (c:id) TokId : ourLexer rest')

> lexChar :: String -> [Token]
> lexChar rest 
>   = lexReadChar rest (\ id rest' -> TokenInfo ("'" ++ id ++ "'") TokId
>                                   : ourLexer rest')
> lexString :: String -> [Token]
> lexString rest = lexReadString rest 
>       (\ id rest' -> TokenInfo ("\"" ++ id ++ "\"") TokId : ourLexer rest')

> lexCode :: String -> [Token]
> lexCode rest = lexReadCode rest 0
>       (\ code rest' -> TokenInfo code TokCodeQuote : ourLexer rest')

This has to match for @}@ that are {\em not} in strings.  The code
here is a bit tricky, but should work in most cases.

> lexReadCode :: String -> Int -> (String -> String -> a) -> a
> lexReadCode ('}':r) n fn
>       | n == 0    = fn "" r
>       | otherwise = lexReadCode r (n-1) (fn . (:) '}')
> lexReadCode ('{':r) n fn = lexReadCode r (n+1) (fn . (:) '{') 
> lexReadCode ('"':r) n fn
>       = lexReadString r (\ str r' -> 
>         lexReadCode r' n (fn . (:) '"'. (++) str . (:) '"') )
> lexReadCode (a:'\'':r) n fn
>	| isAlphanum a = lexReadCode r n (fn . (:) a . (:) '\'')
> lexReadCode ('\'':r) n fn
>	= lexReadChar r (\ str r' -> 
>         lexReadCode r' n (fn . (:) '\''. (++) str . (:) '\'') )
> lexReadCode (c:r) n fn = lexReadCode r n (fn . (:) c)
> lexReadCode []    _ _  
>       = error "Lexical Error: No closing '}' in code segment\n"

%----------------------------------------------------------------------------

\subsection{Utilities that read the rest of a @Token@}

> readId :: String -> (String -> String -> a) -> a
> readId (c:r) fn | isIdPart c = readId r (fn . (:) c)
> readId r     fn = fn [] r

> isIdPart :: Char -> Bool
> isIdPart c =
>	   c >= 'a' && c <= 'z' 
>	|| c >= 'A' && c <= 'Z' 
>	|| c >= '0' && c <= '9' 
>	|| c == '_'

> readInt :: String -> (String -> String -> a) -> a
> readInt (c:r) fn | isNumPart c = readId r (fn . (:) c)
> readInt r     fn               = fn [] r

> isNumPart :: Char -> Bool
> isNumPart x = x >= '0' && x <= '9'

> lexReadChar :: String -> (String -> String -> a) -> a
> lexReadChar ('\'':r)      fn = fn "" r
> lexReadChar ('\\':'\'':r) fn = lexReadChar r (fn . (:) '\\' . (:) '\'')
> lexReadChar ('\\':c:r)    fn = lexReadChar r (fn . (:) '\\' . (:) c)
> lexReadChar (c:r)         fn = lexReadChar r (fn . (:) c)
> lexReadChar []            fn = fn "" []

> lexReadString :: String -> (String -> String -> a) -> a
> lexReadString ('"':r)      fn = fn "" r
> lexReadString ('\\':'"':r) fn = lexReadString r (fn . (:) '\\' . (:) '"')
> lexReadString ('\\':c:r)   fn = lexReadString r (fn . (:) '\\' . (:) c)
> lexReadString (c:r)        fn = lexReadString r (fn . (:) c)
> lexReadString []           fn = fn "" []
