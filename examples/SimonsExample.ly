> {
> module Parser (parse) where
> import Type
> import Lexer
> }

> %token 
> 	backslash	{ Builtin "\\" }
>  	var		{ Ident $$ }
>  	rightarrow	{ Builtin "->" }
>  	caseT		{ Builtin "case" }
>  	letT		{ Builtin "let" }
>  	ofT		{ Builtin "of" }
>  	inT		{ Builtin "in" }
>  	letnT		{ Builtin "letn" }
>  	leftcurly	{ LeftCurly }
>  	rightcurly	{ RightCurly }
>  	equals		{ Builtin "=" }
>  	colon		{ Builtin ":" }
>  	cons		{ Constructor $$ }
>  	leftbracket	{ LeftBracket }
>  	rightbracket	{ RightBracket }
>  	semicolon	{ SemiColon }
>  	percent		{ Percent }

> %name parse
> %tokentype { Token }

> %%

> expr		
> 	: backslash var binders rightarrow expr
> 					{ foldr Lambda $5 ($2: reverse $3) }
> 	| caseT var ofT leftcurly patterns rightcurly
> 					{ Case $2 (reverse $5) }
> 	| letT var equals var expr inT expr
> 					{ LetApp ($2,$4,$5) $7 }
> 	| letT var equals expr inT expr
> 					{ Let ($2,$4) $6 }
> 	| letnT var equals expr inT expr
>					{ LetN ($2,$4) $6 }
> 
> 	| labelref colon expr		{ Label $1 $3 }
>	| simpleexpr			{ $1 }

> simpleexpr
> 	 : cons simpleexprs	{ Cons $1 (reverse $2) }
>	 | simpleexpr0			{ $1 }
> 
> simpleexprs
> 	: simpleexprs simpleexpr0	{ $2 : $1 }
> 	|				{ [] }
> 
> simpleexpr0
> 	: var				{ Var $1 }
> 	| labelref			{ LabelRef $1 }
> 	| leftbracket expr rightbracket { $2 }
> 
> patterns
> 	: patterns pattern		{ $2 : $1 }
> 	| pattern			{ [ $1 ] }
> 
> pattern	: cons binders rightarrow expr semicolon
> 					{ ($1, reverse $2, $4) }
> 
> binders	: binders var			{ $2 : $1 }
> 	| 				{ [ ] }
> 
> labelref
>	: percent var			{ $2 }

> {
> happyError :: Int -> a
> happyError x = error ("Error at LINE " ++ show x)
> }
