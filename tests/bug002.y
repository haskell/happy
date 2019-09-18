#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

{
module Main where

}

%name parser
%token foo { 1 }
%tokentype { QUALIFY(Int) }

%%

-- two productions for the same non-terminal should work
Foo : {- empty -} { () }
Foo : Foo foo     { () }

{
main = QUALIFY(return) ()
happyError = QUALIFY(undefined)
}
