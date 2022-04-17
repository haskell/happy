-----------------------------------------------------------------------------
$Id: ErlParser.ly,v 1.2 1997/09/24 10:11:23 simonm Exp $

Syntactic analyser for Erlang

Copyright : (c) 1996 Ellemtel Telecommunications Systems Laboratories, Sweden
Author    : Simon Marlow <simonm@dcs.gla.ac.uk>
-----------------------------------------------------------------------------

> {
> module Parser (parse) where
> import GenUtils
> import Lexer
> import AbsSyn
> import Types
> import ParseMonad.Class
> }

> %token
> 	atom		{ T_Atom $$ }
> 	var		{ T_Var $$ }
>	int		{ T_Int $$ }
>	float		{ T_Float $$ }
>	string		{ T_String $$ }

> 	'bor'		{ T_Bor }
> 	'bxor'		{ T_Bxor }
>	'bsl'		{ T_Bsl }
>	'bsr'		{ T_Bsr }
>	'div'		{ T_Div }
>	'rem'		{ T_Rem }
>	'band'		{ T_Band }
>	'bnot'		{ T_Bnot }
>	'begin'    	{ T_Begin }
>	'end'    	{ T_End }
>	'catch'    	{ T_Catch }
>	'case'    	{ T_Case }
>	'of'    	{ T_Of }
>	'if'    	{ T_If }
>	'receive'    	{ T_Receive }
>	'after'    	{ T_After }
>	'when'    	{ T_When }
>	'fun'		{ T_Fun }
>	'true'    	{ T_True }
>	'deftype'	{ T_DefType }
>	'type'		{ T_Type }

> 	'+'		{ T_Plus }
> 	'-'		{ T_Minus }
> 	'*'		{ T_Mult }
> 	'/'		{ T_Divide }
> 	'=='		{ T_Eq }
> 	'/='		{ T_Neq }
> 	'=<'		{ T_Leq }
> 	'<'		{ T_Lt }
> 	'>='		{ T_Geq }
> 	'>'		{ T_Gt }
> 	'=:='		{ T_ExactEq }
> 	'=/='		{ T_ExactNeq } 

> 	'!'		{ T_Pling }
> 	'='		{ T_Equals }
> 	'['		{ T_LSquare }
> 	']'		{ T_RSquare }
> 	'('		{ T_LParen }
> 	')'		{ T_RParen }
> 	'{'		{ T_LCurly }
> 	'}'		{ T_RCurly }
> 	','		{ T_Comma }
> 	';'		{ T_SemiColon }
> 	'|'		{ T_Bar }
> 	':'		{ T_Colon }
> 	'->'		{ T_Arrow }
> 	'.'		{ T_Dot }
>	'\\'		{ T_BackSlash }

>	header_prog	{ T_Prog }
>	header_iface	{ T_Interface }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { T_EOF }
> %name parse
> %tokentype	{ Token }

> %%

> parse :: { ProgOrInterface }
> 	: header_prog program		{ It's_a_prog   $2 }
> 	| header_iface interface	{ It's_an_iface $2 }

> program :: { [Form] }
> 	: 				{ [] }
>	| form program			{ $1 : $2 }

> add_op :: { BinOp }
> 	: '+'				{ O_Add }
>	| '-'				{ O_Subtract }
>	| 'bor'				{ O_Bor }
>	| 'bxor'			{ O_Bxor }
>	| 'bsl'				{ O_Bsl }
>	| 'bsr'				{ O_Bsr }

> comp_op :: { CompOp }
> 	: '=='				{ O_Eq }
>	| '/='				{ O_Neq }
>	| '=<'				{ O_Leq }
>	| '<'				{ O_Lt }
>	| '>='				{ O_Geq }
>	| '>'				{ O_Gt }
>	| '=:='				{ O_ExactEq }
>	| '=/='				{ O_ExactNeq }

> mult_op :: { BinOp }
> 	: '*'				{ O_Multiply }
>	| '/'				{ O_Divide }
>	| 'div'				{ O_Div }
>	| 'rem'				{ O_Rem }
>	| 'band'			{ O_Band }

> prefix_op :: { UnOp }
> 	: '+'				{ O_Plus }
>	| '-'				{ O_Negate }
>	| 'bnot'			{ O_Bnot }

> basic_type :: { Expr }
> 	: atm				{ E_Atom $1 }
>	| int				{ E_Int $1 }
>	| float				{ E_Float $1 }
>	| string		{ foldr E_Cons E_Nil (map (E_Int . ord) $1) }
>	| var				{ E_Var $1 }

> pattern :: { Expr }
> 	: basic_type			{ $1 }
>	| '[' ']'			{ E_Nil }
>	| '[' pattern pat_tail ']'	{ E_Cons $2 $3 }
>	| '{' '}'			{ E_Tuple [] }
>	| '{' patterns '}'		{ E_Tuple $2 }
>	| atm '{' patterns '}'		{ E_Struct $1 $3 }

> pat_tail :: { Expr }
> 	: '|' pattern			{ $2 }
>	| ',' pattern pat_tail		{ E_Cons $2 $3 }
>	|				{ E_Nil }

> patterns :: { [ Expr ] }
> 	: pattern			{ [ $1 ] }
>	| pattern ',' patterns		{ $1 : $3 }

> expr :: { Expr }
>	: 'catch' expr			{ E_Catch $2 }
>	| 'fun' '(' formal_param_list ')' '->' expr 'end' { E_Fun $3 $6 }
>	| 'fun' var '/' int		{ E_FunName (LocFun $2 $4) }
>	| 'fun' var ':' var '/' int	{ E_FunName (ExtFun $2 $4 $6) }
>	| expr200			{ $1 }

> expr200 :: { Expr }
>	: expr300 '=' expr		{ E_Match $1 $3 }
>	| expr300 '!' expr		{ E_Send $1 $3 }
>	| expr300			{ $1 }

> expr300 :: { Expr }
> 	: expr300 add_op expr400	{ E_BinOp $2 $1 $3 }
>	| expr400			{ $1 }

> expr400 :: { Expr }
> 	: expr400 mult_op expr500	{ E_BinOp $2 $1 $3 }
>	| expr500			{ $1 }

> expr500 :: { Expr }
> 	: prefix_op expr0		{ E_UnOp $1 $2 }
>	| expr0				{ $1 }

> expr0 :: { Expr }
> 	: basic_type				{ $1 }
> 	| '[' ']'				{ E_Nil }
>	| '[' expr expr_tail ']'		{ E_Cons $2 $3 }
>	| '{' maybeexprs '}'			{ E_Tuple $2 }
>	| atm '{' maybeexprs '}'		{ E_Struct $1 $3 }
> 	| atm '(' maybeexprs ')'  { E_Call (LocFun $1 (length $3)) $3 }
>	| atm ':' atm '(' maybeexprs ')' 
>				  { E_Call (ExtFun $1 $3 (length $5)) $5 }
>	| '(' expr ')'				{ $2 }
>	| 'begin' exprs 'end'			{ E_Block $2 }
>	| 'case' expr 'of' cr_clauses 'end'  	{ E_Case $2 $4 }
>	| 'if' if_clauses 'end'			{ E_If $2 }
> 	| 'receive' 'after' expr '->' exprs 'end'
>					{ E_Receive [] (Just ($3,$5)) }
>	| 'receive' cr_clauses 'end'	{ E_Receive $2 Nothing }
>	| 'receive' cr_clauses 'after' expr '->' exprs 'end'
>					{ E_Receive $2 (Just ($4,$6)) }

> expr_tail :: { Expr }
> 	: '|' expr			{ $2 }
>	| ',' expr expr_tail		{ E_Cons $2 $3 }
>	| 				{ E_Nil }

> cr_clause :: { CaseClause }
> 	: expr clause_guard '->' exprs 	{ ($1,$2,$4) }

> clause_guard :: { [ GuardTest ] }
> 	: 'when' guard			{ $2 }
>	|				{ [] }

> cr_clauses :: { [ CaseClause ] }
> 	: cr_clause			{ [ $1 ] }
>	| cr_clause ';' cr_clauses	{ $1 : $3 }

> if_clause :: { IfClause }
> 	: guard '->' exprs		{ ($1,$3) }

> if_clauses :: { [ IfClause ] }
> 	: if_clause			{ [ $1 ] }
>	| if_clause ';' if_clauses	{ $1 : $3 }

> maybeexprs :: { [ Expr ] }
>	: exprs				{ $1 }
>	|				{ [] }

> exprs :: { [ Expr ] }
> 	: expr				{ [ $1 ] }
>	| expr ',' exprs		{ $1 : $3 }

> guard_test :: { GuardTest }
> 	: atm '(' maybeexprs ')' 	{ G_Bif $1 $3 }
>	| expr300 comp_op expr300       { G_Cmp $2 $1 $3 }

> guard_tests :: { [ GuardTest ] }
> 	: guard_test			{ [ $1 ] }
>	| guard_test ',' guard_tests	{ $1 : $3 }

> guard :: { [ GuardTest ] }
> 	: 'true'			{ [] }
>	| guard_tests			{ $1 }

> function_clause :: { FunctionClause }
> 	: atm '(' formal_param_list ')' clause_guard '->' exprs
>					{ (LocFun $1 (length $3),$3,$5,$7) }

> formal_param_list :: { [ Expr ] }
>	:				{ [] }
> 	| patterns			{ $1 }

> function :: { Function }
> 	: function_clause		{ [ $1 ] }
>	| function_clause ';' function	{ $1 : $3 }

> attribute :: { Attribute }
> 	: pattern			{ A_Pat $1 }
>	| '[' farity_list ']'		{ A_Funs $2 }
>	| atm ',' '[' maybe_farity_list ']'	{ A_AtomAndFuns $1 $4 }

> maybe_farity_list :: { [ Fun ] }
> 	: farity_list			{ $1 }
>	| 				{ [] }

> farity_list :: { [ Fun ] }
> 	: farity			{ [ $1 ] }
>	| farity ',' farity_list	{ $1 : $3 }

> farity :: { Fun }
> 	: atm '/' int			{ LocFun $1 $3 }

> form :: { Form }
> 	: '-' atm '(' attribute ')' '.'  { F_Directive $2 $4 }
>	| '-' 'type' sigdef '.'		 { $3 }
>	| '-' 'deftype' deftype '.'	 { $3 }
>	| function '.'			 { F_Function $1 }

> abstype :: { Form }
>	: atm '(' maybetyvars ')' maybeconstraints	
>		{ F_AbsTypeDef (Tycon $1 (length $3)) $3 (snd $5) }

> deftype :: { Form }
> 	: atm '(' maybetyvars ')' '=' utype maybeconstraints
>		{ F_TypeDef (Tycon $1 (length $3)) $3 $6 (fst $7) (snd $7) }

> sigdef :: { Form }
> 	: atm '(' maybeutypes ')' '->' utype maybeconstraints
>		{ F_TypeSig  ($1,length $3) $3 $6 (fst $7) (snd $7) }

> header :: { (String,Int,[UType]) }
>	 : atm '(' maybeutypes ')'		{ ($1, length $3, $3) }

> tycon_args :: { [ TyVar ] }
>	: tycon_args ',' var			{ STyVar $3 : $1 }
> 	| var					{ [ STyVar $1 ] }

-----------------------------------------------------------------------------
Interfaces & Types

> interface :: { (Module, [ Form ]) }
> 	: '-' atm '(' atm ')' '.' signatures
>					{ ($4, $7) }

> signatures :: { [ Form ] }
> 	: signatures typedef '.'	{ $2 : $1 }
>	|				{ [] }

> typedef :: { Form }
> typedef
>	: '-' 'deftype' deftype		{ $3 }
>	| '-' 'deftype' abstype		{ $3 }
>	| sigdef			{ $1 }

> maybeconstraints :: { ([Constraint], [VarConstraint]) }
> 	: 'when' constraints 		{ splitConstraints $2 }
>	|				{ ([],[]) }

> constraints :: { [ VarOrTypeCon ] }
> 	: constraints ';' constraint	{ $1 ++ $3 }
>	| constraint			{ $1 }

> constraint :: { [ VarOrTypeCon ] }
> 	: utype '<' '=' utype		{ [TypeCon ($1,$4)] }
>	| utype '=' utype		{ [TypeCon ($1,$3),TypeCon($3,$1)] }
>	| var '\\' tags			{ [VarCon (STyVar $1,(canonTags $3))] }

> maybeutypes :: { [ UType ] }
>	: utypes			{ reverse $1 }
>	|				{ [] }

> utypes :: { [ UType ] }
> 	: utypes ',' utype		{ $3 : $1 }
>	| utype				{ [$1] }

> maybetyvars :: { [ TyVar ] }
>	: tyvars			{ reverse $1 }
>	|				{ [] }

> tyvars :: { [ TyVar ] }
>	: tyvars ',' var		{ STyVar $3 : $1 }
>	| var				{ [ STyVar $1 ] }

> utype :: { UType }
> 	: ptypes			{ U (reverse $1) [] }
>	| ptypes '|' tyvar		{ U (reverse $1) [$3] }
>	| tyvar				{ U [] [$1] }
>	| '(' utype ')'			{ $2 }
>	| '(' ')'			{ U [] [] }

> tyvar :: { TaggedTyVar }
> 	: var				{ TyVar [] (STyVar $1) }
>	| int				{ if $1 /= 1 then
>						error "Illegal type variable"
>					  else universalTyVar }
>	| int '\\' tags			{ if $1 /= 1 then
>						error "Illegal type variable"
>					  else partialUniversalTyVar $3 }

> ptypes :: { [ PType ] }
> 	: ptypes '|' ptype		{ $3 : $1 }
> 	| ptype				{ [$1] }

> ptype :: { PType }
> 	: atm '(' ')'			{ conToType $1 [] }
>	| atm '(' utypes ')'		{ conToType $1 (reverse $3) }
> 	| atm				{ TyAtom $1 }
>	| '{' utypes '}'		{ tytuple (reverse $2) }
>	| atm '{' maybeutypes '}'	{ TyStruct $1 $3 }
>	| '[' utype ']'			{ tylist $2 }

> taglist :: { [ Tag ] }
> 	: taglist ',' tag		{ $3 : $1 }
> 	| tag				{ [ $1 ] }

> tags  :: { [ Tag ] }
>	: tag				{ [ $1 ] }
>	| '(' taglist ')'		{ $2 }

> tag	:: { Tag }
>  	: atm '(' ')'			{ conToTag $1 }
> 	| atm				{ TagAtom $1 }
>	| atm '/' int			{ TagStruct $1 $3 }
>	| '{' int '}'			{ tagtuple $2 }
>	| '[' ']'			{ taglist }

Horrible - keywords that can be atoms too.

> atm	:: { String }
> 	: atom				{ $1 }
> 	| 'true'			{ "true" }
>	| 'deftype'			{ "deftype" }
>	| 'type'			{ "type" }

> {
> utypeToVar (U [] [TyVar [] x]) = x
> utypeToVar _ = error "Type constructor arguments must be variables\n"

> happyError :: P a
> happyError s line = failP (show line ++ ": Parse error\n") s line
> }
