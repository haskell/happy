-----------------------------------------------------------------------------
$Id: Parser.ly,v 1.15 2005/01/26 01:10:42 ross Exp $

The parser.

(c) 1993-2000 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> {
> module Parser (ourParser,AbsSyn) where
> import ParseMonad
> import AbsSyn
> import Lexer
> }

> %name ourParser
> %tokentype { Token }
> %token
>	id		{ TokenInfo $$ TokId }
>	spec_tokentype	{ TokenKW      TokSpecId_TokenType }
>	spec_token	{ TokenKW      TokSpecId_Token }
>	spec_name	{ TokenKW      TokSpecId_Name }
>	spec_partial	{ TokenKW      TokSpecId_Partial }
>	spec_lexer	{ TokenKW      TokSpecId_Lexer }
>	spec_imported_identity	{ TokenKW      TokSpecId_ImportedIdentity }
>	spec_monad	{ TokenKW      TokSpecId_Monad }
>       spec_nonassoc	{ TokenKW      TokSpecId_Nonassoc }
>       spec_left	{ TokenKW      TokSpecId_Left }
>       spec_right	{ TokenKW      TokSpecId_Right }
>       spec_prec	{ TokenKW      TokSpecId_Prec }
>       spec_expect     { TokenKW      TokSpecId_Expect }
>	code		{ TokenInfo $$ TokCodeQuote }
>       int             { TokenNum $$  TokNum }
>	":"		{ TokenKW      TokColon }
>	";"		{ TokenKW      TokSemiColon }
>	"::"		{ TokenKW      TokDoubleColon }
>	"%%"		{ TokenKW      TokDoublePercent }
>	"|"		{ TokenKW      TokBar }

> %monad { P }
> %lexer { lexer } { TokenEOF }

> %%

> parser :: { AbsSyn }
>	: optCode tokInfos "%%" rules optCode
>				{ AbsSyn $1 (reverse $2) (reverse $4) $5 }

> rules :: { [(String, [([String],String,Int,Maybe String)], Maybe String)] }
> 	: rules rule	{ $2 : $1 }
>	| rule		{ [$1] }

> rule :: { (String, [([String],String,Int,Maybe String)], Maybe String) }
> 	: id "::" code ":" prods	{ ($1,$5,Just $3) }
>	| id "::" code id ":" prods	{ ($1,$6,Just $3) }
>  	| id ":" prods			{ ($1,$3,Nothing) }


> prods :: { [([String],String,Int,Maybe String)] }
> 	: prod "|" prods		{ $1 : $3 }
>	| prod				{ [$1] }

> prod :: { ([String],String,Int,Maybe String) }
> 	: ids prec code ";"		{% lineP >>= \l -> return ($1,$3,l,$2) }
>	| ids prec code			{% lineP >>= \l -> return ($1,$3,l,$2) }

> prec :: { Maybe String }
>       : spec_prec id			{ Just $2 }
>       |            			{ Nothing }

> tokInfos :: { [Directive String] } 
>	: tokInfos tokInfo		{ $2 : $1 }
>	| tokInfo			{ [$1] }

> tokInfo :: { Directive String } 
>	: spec_tokentype code		{ TokenType $2 }
>	| spec_token tokenSpecs		{ TokenSpec $2 }
>	| spec_name id optStart		{ TokenName $2 $3 False }
>	| spec_partial id optStart	{ TokenName $2 $3 True  }
>	| spec_imported_identity	{ TokenImportedIdentity }
>	| spec_lexer code code		{ TokenLexer $2 $3 }
>	| spec_monad code		{ TokenMonad "()" $2 ">>=" "return" }
>	| spec_monad code code		{ TokenMonad $2 $3 ">>=" "return" }
>	| spec_monad code code code	{ TokenMonad "()" $2 $3 $4 }
>	| spec_monad code code code code	{ TokenMonad $2 $3 $4 $5 }
>	| spec_nonassoc ids		{ TokenNonassoc $2 }
>	| spec_right ids		{ TokenRight $2 }
>	| spec_left ids			{ TokenLeft $2 }
>       | spec_expect int               { TokenExpect $2 }

> optStart :: { Maybe String }
> 	: id				{ Just $1 }
>	| {- nothing -}			{ Nothing }

> tokenSpecs :: { [(String,String)] }
>	: tokenSpec tokenSpecs		{ $1:$2 }
>	| tokenSpec 			{ [$1] }

> tokenSpec :: { (String,String) }
>	: id code			{ ($1,$2) }

> ids 	:: { [String] }
> 	: id ids			{ $1 : $2 }
>	| {- nothing -}			{ [] }

> optCode :: { Maybe String }
>	: code				{ Just $1 }
>	| {- nothing -}			{ Nothing }

> {
> happyError :: P a
> happyError = lineP >>= \l -> fail (show l ++ ": Parse error\n")
> }
