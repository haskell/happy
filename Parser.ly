> {
> module Parser (ourParser,AbsSyn) where
> import GenUtils
> import AbsSyn
> import Lexer

> }

> %name ourParser
> %tokentype { Token }
> %token
>	id_tok		{ TokenInfo $$ TokId }
>	spec_tokentype	{ TokenKW     TokSpecId_TokenType }
>	spec_token	{ TokenKW     TokSpecId_Token }
>	spec_name	{ TokenKW     TokSpecId_Name }
>	spec_newline	{ TokenKW     TokSpecId_Newline }
>	codequote	{ TokenInfo $$ TokCodeQuote }
>	":"		{ TokenKW     TokColon }
>	";"		{ TokenKW     TokSemiColon }
>	"::"		{ TokenKW     TokDoubleColon }
>	"%%"		{ TokenKW     TokDoublePercent }
>	"|"		{ TokenKW     TokBar }
> %newline            	{ TokenKW     TokNewLine }

> %%

> parser :: { AbsSyn }
> parser
>	: optCode tokInfos "%%" rules optCode
>				{ AbsSyn $1 (reverse $2) (reverse $4) $5 }

> rules :: { [(String, [([String],String,Int)], Maybe String)] }
> rules : rules rule	{ $2 : $1 }
>	| rule		{ [$1] }


> rule :: { (String, [([String],String,Int)], Maybe String) }
> rule 	: id "::" code id ":" prods		{ ($1,$6,Just $3) }
>  	| id ":" prods				{ ($1,$3,Nothing) }


> prods :: { [([String],String,Int)] }
> prods : prod "|" prods			{ $1 : $3 }
>	| prod					{ [$1] }

> prod :: { ([String],String,Int) }
> prod	: ids code ";"	{ ($1,$2,$#) }
>	| ids code 	{ ($1,$2,$#) }

> tokInfos :: { [Directive String] } 
> tokInfos 
>	: tokInfos tokInfo			{ $2 : $1 }
>	| tokInfo				{ [$1] }

> tokInfo :: { Directive String } 
> tokInfo 
>	: spec_tokentype code			{ TokenType $2 }
>	| spec_token tokenSpecs			{ TokenSpec $2 }
>	| spec_name id				{ TokenName $2 }
>	| spec_newline	code			{ TokenNewline $2 }

> tokenSpecs :: { [(String,String)] }
> tokenSpecs 
>	: tokenSpec tokenSpecs
>			{ $1:$2 }
>	| tokenSpec 	{ [$1] }

> tokenSpec :: { (String,String) }
> tokenSpec 
>	: id code	{ ($1,$2) }

> ids 	:: { [String] }
> ids	: id ids	{ $1 : $2 }
>	| 		{ [] }

> id 	:: { String }
> id	: id_tok	{ $1 }

> optCode :: { Maybe String }
> optCode 
>	: code		{ Just $1 }
>	| 		{ Nothing }

> code :: { String }
> code
>	: codequote	{ $1 }


> {

> happyError :: Int -> [Token] -> a
> happyError i (t:ts) = 
> 	error ("Parse error in line " ++ show i ++ "\n")

> }
