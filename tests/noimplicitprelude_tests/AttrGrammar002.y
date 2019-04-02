{
{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
}


%tokentype { Prelude.Char }

%token minus { '-' }
%token plus  { '+' }
%token one   { '1' }
%token zero  { '0' }

%attributetype { Attrs }
%attribute value { Prelude.Integer }
%attribute pos   { Prelude.Int }

%name parse start

%monad { Prelude.Maybe }

%%

start
   : num { $$ = $1 }

num
   : bits        { $$ = $1       ; $1.pos = 0 }
   | plus bits   { $$ = $2       ; $2.pos = 0 }
   | minus bits  { $$ = Prelude.negate $2; $2.pos = 0 }

bits
   : bit         { $$ = $1
                 ; $1.pos = $$.pos
                 }

   | bits bit    { $$ = $1 Prelude.+ $2
                 ; $1.pos = $$.pos Prelude.+ 1
                 ; $2.pos = $$.pos
                 }

bit
   : zero        { $$ = 0 }
   | one         { $$ = 2 Prelude.^ ($$.pos) }


{
happyError msg = Prelude.fail Prelude.$ "parse error: " Prelude.++ msg

main = case parse ""      of { Prelude.Nothing ->
       case parse "abc"   of { Prelude.Nothing ->
       case parse "0"     of { Prelude.Just 0  ->
       case parse "1"     of { Prelude.Just 1  ->
       case parse "101"   of { Prelude.Just 5  ->
       case parse "111"   of { Prelude.Just 7  ->
       case parse "10001" of { Prelude.Just 17 ->
       Prelude.putStrLn "Test worked";
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }

quit = Prelude.putStrLn "Test Failed"
}
