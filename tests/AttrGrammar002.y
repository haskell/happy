
%tokentype { Char }

%token minus { '-' }
%token plus  { '+' }
%token one   { '1' }
%token zero  { '0' }

%attributetype { Attrs }
%attribute value { Integer }
%attribute pos   { Int }

%name parse start

%monad { Maybe }

%%

start 
   : num { $$ = $1 }

num 
   : bits        { $$ = $1       ; $1.pos = 0 }
   | plus bits   { $$ = $2       ; $2.pos = 0 }
   | minus bits  { $$ = negate $2; $2.pos = 0 }

bits
   : bit         { $$ = $1
                 ; $1.pos = $$.pos 
                 }

   | bits bit    { $$ = $1 + $2
                 ; $1.pos = $$.pos + 1
                 ; $2.pos = $$.pos
                 }

bit
   : zero        { $$ = 0 }
   | one         { $$ = 2^($$.pos) }


{
happyError msg = fail $ "parse error: "++msg

main = case parse ""      of { Nothing ->
       case parse "abc"   of { Nothing ->
       case parse "0"     of { Just 0  ->
       case parse "1"     of { Just 1  ->
       case parse "101"   of { Just 5  ->
       case parse "111"   of { Just 7  ->
       case parse "10001" of { Just 17 ->
       putStrLn "Test worked";
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }

quit = putStrLn "Test Failed"
}
