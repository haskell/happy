-----------------------------------------------------------------------------
$Id: Parser.ly,v 1.4 1997/09/09 16:31:49 simonm Exp $

The parser.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> {
> module Parser (ourParser,AbsSyn) where
> import ParseMonad
> import GenUtils
> import AbsSyn
> import Lexer

> #if __HASKELL1__ >= 3 && ( !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ >= 200 )
> import GlaExts
> #endif
> }

> %name ourParser
> %tokentype { Token }
> %token
>	id_tok		{ TokenInfo $$ TokId }
>	spec_tokentype	{ TokenKW     TokSpecId_TokenType }
>	spec_token	{ TokenKW     TokSpecId_Token }
>	spec_name	{ TokenKW     TokSpecId_Name }
>	spec_lexer	{ TokenKW     TokSpecId_Lexer }
>	spec_monad	{ TokenKW     TokSpecId_Monad }
>	codequote	{ TokenInfo $$ TokCodeQuote }
>	":"		{ TokenKW     TokColon }
>	";"		{ TokenKW     TokSemiColon }
>	"::"		{ TokenKW     TokDoubleColon }
>	"%%"		{ TokenKW     TokDoublePercent }
>	"|"		{ TokenKW     TokBar }
>	"("		{ TokenKW     TokOpenBrack }
>	")"		{ TokenKW     TokCloseBrack }
>	"-"		{ TokenKW     TokMinus }
>	"+"		{ TokenKW     TokPlus }
>	"*"		{ TokenKW     TokStar }
>	"?"		{ TokenKW     TokQmark }
>	"\?"		{ TokenKW     TokBackQ }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { TokenEOF }

> %%

> parser :: { AbsSyn }
>	: optCode tokInfos "%%" rules optCode
>				{ AbsSyn $1 (reverse $2) (reverse $4) $5 }

> rules :: { [(String, [([String],String,Int)], Maybe String)] }
> 	: rules rule	{ $2 : $1 }
>	| rule		{ [$1] }

> rule :: { (String, [([String],String,Int)], Maybe String) }
> 	: id "::" code ":" prods		{ ($1,$5,Just $3) }
>	| id "::" code id ":" prods		{ ($1,$6,Just $3) }
>  	| id ":" prods				{ ($1,$3,Nothing) }


> prods :: { [([String],String,Int)] }
> 	: prod "|" prods			{ $1 : $3 }
>	| prod					{ [$1] }

> prod :: { ([String],String,Int) }
> 	: ids code ";"	{% \s l -> returnP ($1,$2,l) s l }
>	| ids code 	{% \s l -> returnP ($1,$2,l) s l }

> tokInfos :: { [Directive String] } 
>	: tokInfos tokInfo			{ $2 : $1 }
>	| tokInfo				{ [$1] }

> tokInfo :: { Directive String } 
>	: spec_tokentype code			{ TokenType $2 }
>	| spec_token tokenSpecs			{ TokenSpec $2 }
>	| spec_name id				{ TokenName $2 }
>	| spec_lexer code code			{ TokenLexer $2 $3 }
>	| spec_monad code code code		{ TokenMonad $2 $3 $4 }

> tokenSpecs :: { [(String,String)] }
>	: tokenSpec tokenSpecs
>			{ $1:$2 }
>	| tokenSpec 	{ [$1] }

> tokenSpec :: { (String,String) }
>	: id code		{ ($1,$2) }

> ids 	:: { [String] }
> 	: id ids	{ $1 : $2 }
>	| 		{ [] }

> id 	:: { String }
> 	: id_tok	{ $1 }

> optCode :: { Maybe String }
>	: code		{ Just $1 }
>	| 		{ Nothing }

> code :: { String }
>	: codequote	{ $1 }


> {

> happyError :: P a
> happyError s l = failP (show l ++ ": Parse error\n") s l

> }
