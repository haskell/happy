{
import Control.Monad (unless)
import System.Exit (exitFailure)
}

%tokentype { Char }

%token a { 'a' }
%token b { 'b' }
%token c { 'c' }

%attributetype { Attrs a }
%attribute value { a }
%attribute len   { Int }

%name parse abcstring

%monad { Maybe }

%%

abcstring 
   : alist blist clist
        { $$ = $1 ++ $2 ++ $3
        ; $2.len = $1.len
        ; $3.len = $1.len
        }

alist 
   : a alist 
        { $$ = $1 : $>
        ; $$.len = $>.len + 1
        }
   |    { $$ = []; $$.len = 0 }

blist 
   : b blist
        { $$ = $1 : $>
        ; $>.len = $$.len - 1
        }
   |    { $$ = []
        ; where failUnless ($$.len == 0) "blist wrong length" 
        }

clist
   : c clist
        { $$ = $1 : $>
        ; $>.len = $$.len - 1
        }
   |    { $$ = []
        ; where failUnless ($$.len == 0) "clist wrong length" 
        }

{
happyError = error "parse error"
failUnless b msg = unless b (fail msg)

main = case parse "" of { Just _ -> 
       case parse "abc" of { Just _ ->
       case parse "aaaabbbbcccc" of { Just _ ->
       case parse "abbcc" of { Just _ ->
       case parse "aabcc" of { Nothing ->
       case parse "aabbc" of { Nothing ->
       putStrLn "Test works";
       _ -> quit } ; _ -> quit }; _ -> quit };
       _ -> quit } ; _ -> quit }; _ -> quit }

quit = putStrLn "Test failed" >> exitFailure
}
