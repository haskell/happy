Parses simple lambda expressions to combinators

> { 

> module Parser where

> import Lexer
> import Convert
> import PreludeGlaArray

> }

> %name parse
> %tokentype { Token }
> %token idT		{ Ident $$ }
>        numT		{ Number $$ }
>        boolT		{ Boolean $$ }
>        "("		{ LeftBracket }
>        ")"		{ RightBracket }
>        "["		{ LeftSquare }
>        "]"		{ RightSquare }
>        "[]"		{ EmptyList }
>	 ";"		{ SemiColon }
>	 ":"		{ Colon }
>        "+"		{ Infix "+" }
>        "-"		{ Infix "-" }
>        "/"		{ Infix "/" }
>        "*"		{ Infix "*" }
>        "=="		{ Infix "==" }
>        "/="		{ Infix "/=" }
>        ">"		{ Infix ">" }
>        "<"		{ Infix "<" }
>        ">="		{ Infix ">=" }
>        "<="		{ Infix "<=" }
>	 "="		{ Builtin "=" }
>        "else"		{ Builtin "else" }
>        "if"		{ Builtin "if" }
>	 "in"		{ Builtin "in" }
>	 "let"		{ Builtin "let" }
>	 "then"		{ Builtin "then" }
>	 "end"		{ Builtin "end" }
> %%

> P : "let" Dec "in" B		{ mkLet $2 $4}
>   | "if" B "then" B "else" B	{ mkIf $2 $4 $6}
>   | B				{ $1 }

> B :: { Seq (Ptr Exp) }
> B : E "==" E			{ mkOp $1 Equ $3 }
>   | E "/=" E			{ mkOp $1 NEq $3 }
>   | E ">" E			{ mkOp $1 GT $3 }
>   | E "<" E			{ mkOp $1 LT $3 }
>   | E ">=" E			{ mkOp $1 GTE $3 }
>   | E "<=" E			{ mkOp $1 LTE $3 }
>   | E				{ $1 }

> E :: { Seq (Ptr Exp) }
> E : E "+" T			{ mkOp $1 Add $3}
>   | E "-" T			{ mkOp $1 Sub $3}
>   | T                         { $1 }

> T :: { Seq (Ptr Exp) }
> T : T "*" F			{ mkOp $1 Mul $3 }
>   | T "/" F			{ mkOp $1 Quo $3 }
>   | F                      	{ $1 }

> F :: { Seq (Ptr Exp) }
> F : "(" B ")"			{ $2 }
>      | numT			{ mkNum $1 }
>      | boolT			{ mkBool $1 }
>      | idT			{ newPtr (mkVar $1) }
>      | Apps			{ mkApps $1 }

> Apps :: { Seq [Ptr Exp] }
> Apps : F Apps			{ mkApp $1 $2 }
>      | F			{ mkAtom $1 }

> Dec :: { (Token,Seq (Ptr Exp)) }
> Dec : idT Args "=" B		{ ($1, mkFun $1 $2 $4) }

> Args :: { [Exp] }
> Args : idT Args		{ mkVar $1 : $2}
>      | 			{ [] }

> {

> happyError :: Text a => a -> b
> happyError x = error ("Parse error, line " ++ show x ++ "\n")

> }
