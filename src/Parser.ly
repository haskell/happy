-----------------------------------------------------------------------------
$Id: Parser.ly,v 1.9 2000/07/12 16:21:44 simonmar Exp $

The parser.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> {
> module Parser (ourParser,AbsSyn) where
> import ParseMonad
> import GenUtils
> import AbsSyn
> import Lexer

> #ifdef __GLASGOW_HASKELL__
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
>       spec_nonassoc	{ TokenKW     TokSpecId_Nonassoc }
>       spec_left	{ TokenKW     TokSpecId_Left }
>       spec_right	{ TokenKW     TokSpecId_Right }
>       spec_prec	{ TokenKW     TokSpecId_Prec }
>	codequote	{ TokenInfo $$ TokCodeQuote }
>	":"		{ TokenKW     TokColon }
>	";"		{ TokenKW     TokSemiColon }
>	"::"		{ TokenKW     TokDoubleColon }
>	"%%"		{ TokenKW     TokDoublePercent }
>	"|"		{ TokenKW     TokBar }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { TokenEOF }

> %%

> parser :: { AbsSyn }
>	: optCode tokInfos "%%" rules optCode
>				{ AbsSyn $1 (reverse $2) (reverse $4) $5 }

> rules :: { [(String, [([String],String,Int,Maybe String)], Maybe String)] }
> 	: rules rule	{ $2 : $1 }
>	| rule		{ [$1] }

> rule :: { (String, [([String],String,Int,Maybe String)], Maybe String) }
> 	: id "::" code ":" prods		{ ($1,$5,Just $3) }
>	| id "::" code id ":" prods		{ ($1,$6,Just $3) }
>  	| id ":" prods				{ ($1,$3,Nothing) }


> prods :: { [([String],String,Int,Maybe String)] }
> 	: prod "|" prods			{ $1 : $3 }
>	| prod					{ [$1] }

> prod :: { ([String],String,Int,Maybe String) }
> 	: ids prec code ";"	{% \s l -> returnP ($1,$3,l,$2) s l }
>	| ids prec code		{% \s l -> returnP ($1,$3,l,$2) s l }

> prec :: { Maybe String }
>       : spec_prec id	{ Just $2 }
>       |            	{ Nothing }

> tokInfos :: { [Directive String] } 
>	: tokInfos tokInfo			{ $2 : $1 }
>	| tokInfo				{ [$1] }

> tokInfo :: { Directive String } 
>	: spec_tokentype code			{ TokenType $2 }
>	| spec_token tokenSpecs			{ TokenSpec $2 }
>	| spec_name id				{ TokenName $2 }
>	| spec_lexer code code			{ TokenLexer $2 $3 }
>	| spec_monad code		        { TokenMonad $2 ">>=" "return" }
>	| spec_monad code code code		{ TokenMonad $2 $3 $4 }
>	| spec_nonassoc ids			{ TokenNonassoc $2 }
>	| spec_right ids			{ TokenRight $2 }
>	| spec_left ids				{ TokenLeft $2 }

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
