#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{
import Control.Monad (unless)

}

%tokentype { QUALIFIEDPRELUDE.Char }

%token a { 'a' }
%token b { 'b' }
%token c { 'c' }

%attributetype { Attrs a }
%attribute value { a }
%attribute len   { QUALIFIEDPRELUDE.Int }

%name parse abcstring

%monad { QUALIFIEDPRELUDE.Maybe }

%%

abcstring
   : alist blist clist
        { $$ = $1 QUALIFIEDPRELUDE.++ $2 QUALIFIEDPRELUDE.++ $3
        ; $2.len = $1.len
        ; $3.len = $1.len
        }

alist
   : a alist
        { $$ = $1 : $>
        ; $$.len = $>.len QUALIFIEDPRELUDE.+ 1
        }
   |    { $$ = []; $$.len = 0 }

blist
   : b blist
        { $$ = $1 : $>
        ; $>.len = $$.len QUALIFIEDPRELUDE.- 1
        }
   |    { $$ = []
        ; where failUnless ($$.len `equals''` 0) "blist wrong length"
        }

clist
   : c clist
        { $$ = $1 : $>
        ; $>.len = $$.len QUALIFIEDPRELUDE.- 1
        }
   |    { $$ = []
        ; where failUnless ($$.len `equals''` 0) "clist wrong length"
        }

{
happyError = QUALIFIEDPRELUDE.error "parse error"
failUnless b msg = unless b (QUALIFIEDPRELUDE.fail msg)

equals'' a b = a QUALIFIEDPRELUDE.== b

main = case parse "" of { QUALIFIEDPRELUDE.Just _ ->
       case parse "abc" of { QUALIFIEDPRELUDE.Just _ ->
       case parse "aaaabbbbcccc" of { QUALIFIEDPRELUDE.Just _ ->
       case parse "abbcc" of { QUALIFIEDPRELUDE.Nothing ->
       case parse "aabcc" of { QUALIFIEDPRELUDE.Nothing ->
       case parse "aabbc" of { QUALIFIEDPRELUDE.Nothing ->
       QUALIFIEDPRELUDE.putStrLn "Test works";
       _ -> quit } ; _ -> quit }; _ -> quit };
       _ -> quit } ; _ -> quit }; _ -> quit }

quit = QUALIFIEDPRELUDE.putStrLn "Test failed"
}
