%tokentype { Token }
%token IDENTIFIER { TokenIdentifier }
       '+'        { TokenPlus       }
%%

Expr : Term                    { 42 }

Term : IDENTIFIER			{ 42 }
     | Expr '+' Term        { 42 }
