#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{
module Main where

}

%name parser
%token foo { 1 }
%tokentype { QUALIFIEDPRELUDE.Int }

%%

-- two productions for the same non-terminal should work
Foo : {- empty -} { () }
Foo : Foo foo     { () }

{
main = QUALIFIEDPRELUDE.return ()
happyError = QUALIFIEDPRELUDE.undefined
}
