-----------------------------------------------------------------------------
The lexer.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Frontend.Lexer (
>       Token(..),
>       TokenId(..),
>       HasLexer(..) ) where

> import Happy.Frontend.ParseMonad.Class

> import Data.Char ( isSpace, isAlphaNum, isDigit, digitToInt )

> data Token
>       = TokenInfo String TokenId
>       | TokenNum  Int    TokenId
>       | TokenKW          TokenId
>       | TokenEOF

> tokenToId :: Token -> TokenId
> tokenToId (TokenInfo _ i) = i
> tokenToId (TokenNum _ i) = i
> tokenToId (TokenKW i) = i
> tokenToId TokenEOF = error "tokenToId TokenEOF"

> instance Eq Token where
>       i == i' = tokenToId i == tokenToId i'

> instance Ord Token where
>       i <= i' = tokenToId i <= tokenToId i'

> data TokenId
>       = TokId                 -- words and symbols
>       | TokSpecId_TokenType   -- %tokentype
>       | TokSpecId_Token       -- %token
>       | TokSpecId_Name        -- %name
>       | TokSpecId_Partial     -- %partial
>       | TokSpecId_Lexer       -- %lexer
>       | TokSpecId_ImportedIdentity -- %importedidentity
>       | TokSpecId_Monad       -- %monad
>       | TokSpecId_Nonassoc    -- %nonassoc
>       | TokSpecId_Left        -- %left
>       | TokSpecId_Right       -- %right
>       | TokSpecId_Prec        -- %prec
>       | TokSpecId_Shift       -- %shift
>       | TokSpecId_Expect      -- %expect
>       | TokSpecId_Error       -- %error
>       | TokSpecId_ErrorExpected -- %error.expected
>       | TokSpecId_ErrorHandlerType -- %errorhandlertype
>       | TokSpecId_Attributetype -- %attributetype
>       | TokSpecId_Attribute   -- %attribute
>       | TokCodeQuote          -- stuff inside { .. }
>       | TokColon              -- :
>       | TokSemiColon          -- ;
>       | TokDoubleColon        -- ::
>       | TokDoublePercent      -- %%
>       | TokBar                -- |
>       | TokNum                -- Integer
>       | TokParenL             -- (
>       | TokParenR             -- )
>       | TokComma              -- ,
>       deriving (Eq,Ord,Show)

ToDo: proper text instance here, for use in parser error messages.

> instance HasLexer Token where
>   lexToken = lexer

> lexer :: (Token -> Pfunc a) -> Pfunc a
> lexer cont = lexer'
>   where lexer' "" = cont TokenEOF ""
>         lexer' ('-':'-':r) = lexer' (dropWhile (/= '\n') r)
>         lexer' ('{':'-':r) = \line -> lexNestedComment line lexer' r line
>         lexer' (c:rest) = nextLex cont c rest

> nextLex :: (Token -> Pfunc a) -> Char -> String -> Int -> ParseResult a
> nextLex cont c = case c of
>       '\n'    -> \rest line -> lexer cont rest (line+1)
>       '%'     -> lexPercent cont
>       ':'     -> lexColon cont
>       ';'     -> cont (TokenKW TokSemiColon)

>       '|'     -> cont (TokenKW TokBar)
>       '\''    -> lexChar cont
>       '"'{-"-}-> lexString cont
>       '{'     -> lexCode cont

>       '('     -> cont (TokenKW TokParenL)
>       ')'     -> cont (TokenKW TokParenR)
>       ','     -> cont (TokenKW TokComma)

>       _
>         | isSpace c -> lexer cont
>         |  c >= 'a' && c <= 'z'
>            || c >= 'A' && c <= 'Z' -> lexId cont c
>         | isDigit c -> lexNum cont c
>       _       -> lexError ("lexical error before `" ++ c : "'")

Percents come in two forms, in pairs, or
followed by a special identifier.

> lexPercent :: (Token -> Pfunc a) -> [Char] -> Int -> ParseResult a
> lexPercent cont s = case s of
>       '%':rest -> cont (TokenKW TokDoublePercent) rest
>       't':'o':'k':'e':'n':'t':'y':'p':'e':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_TokenType) rest
>       't':'o':'k':'e':'n':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Token) rest
>       'n':'a':'m':'e':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Name) rest
>       'p':'a':'r':'t':'i':'a':'l':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Partial) rest
>       'i':'m':'p':'o':'r':'t':'e':'d':'i':'d':'e':'n':'t':'i':'t':'y':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_ImportedIdentity) rest
>       'm':'o':'n':'a':'d':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Monad) rest
>       'l':'e':'x':'e':'r':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Lexer) rest
>       'n':'o':'n':'a':'s':'s':'o':'c':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Nonassoc) rest
>       'l':'e':'f':'t':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Left) rest
>       'r':'i':'g':'h':'t':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Right) rest
>       'p':'r':'e':'c':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Prec) rest
>       's':'h':'i':'f':'t':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Shift) rest
>       'e':'x':'p':'e':'c':'t':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Expect) rest
>       'e':'r':'r':'o':'r':'.':'e':'x':'p':'e':'c':'t':'e':'d':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_ErrorExpected) rest
>       'e':'r':'r':'o':'r':'h':'a':'n':'d':'l':'e':'r':'t':'y':'p':'e':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_ErrorHandlerType) rest
>       'e':'r':'r':'o':'r':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Error) rest
>       'a':'t':'t':'r':'i':'b':'u':'t':'e':'t':'y':'p':'e':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Attributetype) rest
>       'a':'t':'t':'r':'i':'b':'u':'t':'e':rest | end_of_id rest ->
>               cont (TokenKW TokSpecId_Attribute) rest
>       _ -> lexError ("unrecognised directive: %" ++
>                               takeWhile (not.isSpace) s) s
>    where
>       end_of_id (c:_) = not (isAlphaNum c)
>       end_of_id []    = True

> lexColon :: (Token -> Pfunc a) -> [Char] -> Int -> ParseResult a
> lexColon cont (':':rest) = cont (TokenKW TokDoubleColon) rest
> lexColon cont rest       = cont (TokenKW TokColon) rest

> lexId :: (Token -> Pfunc a) -> Char -> String -> Int -> ParseResult a
> lexId cont c rest =
>       readId rest (\ ident rest' -> cont (TokenInfo (c:ident) TokId) rest')

> lexChar :: (Token -> Pfunc a) -> String -> Int -> ParseResult a
> lexChar cont rest = lexReadChar rest
>       (\ ident -> cont (TokenInfo ("'" ++ ident ++ "'") TokId))

> lexString :: (Token -> Pfunc a) -> String -> Int -> ParseResult a
> lexString cont rest = lexReadString rest
>       (\ ident -> cont (TokenInfo ("\"" ++ ident ++ "\"") TokId))

> lexCode :: (Token -> Pfunc a) -> String -> Int -> ParseResult a
> lexCode cont rest = lexReadCode rest (0 :: Integer) "" cont

> lexNum :: (Token -> Pfunc a) -> Char -> String -> Int -> ParseResult a
> lexNum cont c rest =
>        readNum rest (\ num rest' ->
>                         cont (TokenNum (stringToInt (c:num)) TokNum) rest')
>  where stringToInt = foldl (\n c' -> digitToInt c' + 10*n) 0

> cleanupCode :: String -> String
> cleanupCode s =
>    dropWhile isSpace (reverse (dropWhile isSpace (reverse s)))

This has to match for @}@ that are {\em not} in strings.  The code
here is a bit tricky, but should work in most cases.

> lexReadCode :: (Eq a, Num a)
>             => String -> a -> String -> (Token -> Pfunc b) -> Int
>             -> ParseResult b
> lexReadCode s n c = case s of
>       '\n':r -> \cont l ->  lexReadCode r n ('\n':c) cont (l+1)
>
>       '{' :r -> lexReadCode r (n+1) ('{':c)
>
>       '}' :r
>               | n == 0    -> \cont -> cont (TokenInfo (
>                               cleanupCode (reverse c)) TokCodeQuote) r
>               | otherwise -> lexReadCode r (n-1) ('}':c)
>
>       '"'{-"-}:r -> lexReadString r (\ str r' ->
>                     lexReadCode r' n ('"' : (reverse str) ++ '"' : c))
>
>       a: '\'':r | isAlphaNum a -> lexReadCode r n ('\'':a:c)
>
>       '\'' :r -> lexReadSingleChar r (\ str r' ->
>                  lexReadCode r' n ((reverse str) ++ '\'' : c))
>
>       ch:r -> lexReadCode r n (ch:c)
>
>       [] -> \_cont -> lexError "No closing '}' in code segment" []

----------------------------------------------------------------------------
Utilities that read the rest of a token.

> readId :: String -> (String -> String -> a) -> a
> readId (c:r) fn | isIdPart c = readId r (fn . (:) c)
> readId r     fn = fn [] r

> readNum :: String -> (String -> String -> a) -> a
> readNum (c:r) fn | isDigit c = readNum r (fn . (:) c)
> readNum r     fn = fn [] r

> isIdPart :: Char -> Bool
> isIdPart c =
>          c >= 'a' && c <= 'z'
>       || c >= 'A' && c <= 'Z'
>       || c >= '0' && c <= '9'
>       || c == '_'

> lexReadSingleChar :: String -> (String -> String -> a) -> a
> lexReadSingleChar ('\\':c:'\'':r) fn = fn ('\\':c:"'") r
> lexReadSingleChar (c:'\'':r)      fn = fn (c:"'") r
> lexReadSingleChar r               fn = fn "" r

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

> lexError :: String -> String -> Int -> ParseResult a
> lexError err = \_ l -> Left (show l ++ ": " ++ err ++ "\n")

> lexNestedComment :: Int -> ([Char] -> Int -> ParseResult a) -> [Char] -> Int
>                  -> ParseResult a
> lexNestedComment l cont r =
>   case r of
>       '-':'}':r' -> cont r'
>       '{':'-':r' -> \line -> lexNestedComment line
>                       (\r'' -> lexNestedComment l cont r'') r' line
>       '\n':r'    -> \line -> lexNestedComment l cont r' (line+1)
>       _:r'       -> lexNestedComment l cont r'
>       ""         -> \_ -> lexError "unterminated comment" r l
