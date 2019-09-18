#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

{
import Control.Monad (unless)

}

%tokentype { QUALIFY(Char) }

%token a { 'a' }
%token b { 'b' }
%token c { 'c' }

%attributetype { Attrs a }
%attribute value { a }
%attribute len   { QUALIFY(Int) }

%name parse abcstring

%monad { QUALIFY(Maybe) }

%%

abcstring
   : alist blist clist
        { $$ = $1 QUALIFY(++) $2 QUALIFY(++) $3
        ; $2.len = $1.len
        ; $3.len = $1.len
        }

alist
   : a alist
        { $$ = $1 : $>
        ; $$.len = $>.len QUALIFY(+) 1
        }
   |    { $$ = []; $$.len = 0 }

blist
   : b blist
        { $$ = $1 : $>
        ; $>.len = $$.len QUALIFY(-) 1
        }
   |    { $$ = []
        ; where failUnless ($$.len `equals''` 0) "blist wrong length"
        }

clist
   : c clist
        { $$ = $1 : $>
        ; $>.len = $$.len QUALIFY(-) 1
        }
   |    { $$ = []
        ; where failUnless ($$.len `equals''` 0) "clist wrong length"
        }

{
happyError = QUALIFY(error) "parse error"
failUnless b msg = unless b (QUALIFY(fail) msg)

equals'' a b = a QUALIFY(==) b

main = case parse "" of { QUALIFY(Just) _ ->
       case parse "abc" of { QUALIFY(Just) _ ->
       case parse "aaaabbbbcccc" of { QUALIFY(Just) _ ->
       case parse "abbcc" of { QUALIFY(Nothing) ->
       case parse "aabcc" of { QUALIFY(Nothing) ->
       case parse "aabbc" of { QUALIFY(Nothing) ->
       QUALIFY(putStrLn) "Test works";
       _ -> quit } ; _ -> quit }; _ -> quit };
       _ -> quit } ; _ -> quit }; _ -> quit }

quit = QUALIFY(putStrLn) "Test failed"
}
