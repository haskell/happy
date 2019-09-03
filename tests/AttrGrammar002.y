#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{

}


%tokentype { QUALIFIEDPRELUDE.Char }

%token minus { '-' }
%token plus  { '+' }
%token one   { '1' }
%token zero  { '0' }

%attributetype { Attrs }
%attribute value { QUALIFIEDPRELUDE.Integer }
%attribute pos   { QUALIFIEDPRELUDE.Int }

%name parse start

%monad { QUALIFIEDPRELUDE.Maybe }

%%

start
   : num { $$ = $1 }

num
   : bits        { $$ = $1       ; $1.pos = 0 }
   | plus bits   { $$ = $2       ; $2.pos = 0 }
   | minus bits  { $$ = QUALIFIEDPRELUDE.negate $2; $2.pos = 0 }

bits
   : bit         { $$ = $1
                 ; $1.pos = $$.pos
                 }

   | bits bit    { $$ = $1 QUALIFIEDPRELUDE.+ $2
                 ; $1.pos = $$.pos QUALIFIEDPRELUDE.+ 1
                 ; $2.pos = $$.pos
                 }

bit
   : zero        { $$ = 0 }
   | one         { $$ = 2 QUALIFIEDPRELUDE.^ ($$.pos) }


{
happyError msg = QUALIFIEDPRELUDE.fail QUALIFIEDPRELUDE.$ "parse error: " QUALIFIEDPRELUDE.++ msg

main = case parse ""      of { QUALIFIEDPRELUDE.Nothing ->
       case parse "abc"   of { QUALIFIEDPRELUDE.Nothing ->
       case parse "0"     of { QUALIFIEDPRELUDE.Just 0  ->
       case parse "1"     of { QUALIFIEDPRELUDE.Just 1  ->
       case parse "101"   of { QUALIFIEDPRELUDE.Just 5  ->
       case parse "111"   of { QUALIFIEDPRELUDE.Just 7  ->
       case parse "10001" of { QUALIFIEDPRELUDE.Just 17 ->
       QUALIFIEDPRELUDE.putStrLn "Test worked";
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }

quit = QUALIFIEDPRELUDE.putStrLn "Test Failed"
}
