{
import Control.Monad (unless)

{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude

}

%tokentype { Prelude.Char }

%token a { 'a' }
%token b { 'b' }
%token c { 'c' }

%attributetype { Attrs a }
%attribute value { a }
%attribute len   { Prelude.Int }

%name parse abcstring

%monad { Prelude.Maybe }

%%

abcstring
   : alist blist clist
        { $$ = $1 Prelude.++ $2 Prelude.++ $3
        ; $2.len = $1.len
        ; $3.len = $1.len
        }

alist
   : a alist
        { $$ = $1 : $>
        ; $$.len = $>.len Prelude.+ 1
        }
   |    { $$ = []; $$.len = 0 }

blist
   : b blist
        { $$ = $1 : $>
        ; $>.len = $$.len Prelude.- 1
        }
   |    { $$ = []
        ; where failUnless ($$.len `prequals` 0) "blist wrong length"
        }

clist
   : c clist
        { $$ = $1 : $>
        ; $>.len = $$.len Prelude.- 1
        }
   |    { $$ = []
        ; where failUnless ($$.len `prequals` 0) "clist wrong length"
        }

{
happyError = Prelude.error "parse error"

failUnless b msg = unless b (Prelude.fail msg)

prequals a b = a Prelude.== b

main = case parse "" of { Prelude.Just _ ->
       case parse "abc" of { Prelude.Just _ ->
       case parse "aaaabbbbcccc" of { Prelude.Just _ ->
       case parse "abbcc" of { Prelude.Nothing ->
       case parse "aabcc" of { Prelude.Nothing ->
       case parse "aabbc" of { Prelude.Nothing ->
       Prelude.putStrLn "Test works";
       _ -> quit } ; _ -> quit }; _ -> quit };
       _ -> quit } ; _ -> quit }; _ -> quit }

quit = Prelude.putStrLn "Test failed"
}
