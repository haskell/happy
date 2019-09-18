#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

{

}


%tokentype { QUALIFY(Char) }

%token minus { '-' }
%token plus  { '+' }
%token one   { '1' }
%token zero  { '0' }

%attributetype { Attrs }
%attribute value { QUALIFY(Integer) }
%attribute pos   { QUALIFY(Int) }

%name parse start

%monad { QUALIFY(Maybe) }

%%

start
   : num { $$ = $1 }

num
   : bits        { $$ = $1       ; $1.pos = 0 }
   | plus bits   { $$ = $2       ; $2.pos = 0 }
   | minus bits  { $$ = QUALIFY(negate) $2; $2.pos = 0 }

bits
   : bit         { $$ = $1
                 ; $1.pos = $$.pos
                 }

   | bits bit    { $$ = $1 QUALIFY(+) $2
                 ; $1.pos = $$.pos QUALIFY(+) 1
                 ; $2.pos = $$.pos
                 }

bit
   : zero        { $$ = 0 }
   | one         { $$ = 2 QUALIFY(^) ($$.pos) }


{
happyError msg = QUALIFY(fail) QUALIFY($) "parse error: " QUALIFY(++) msg

main = case parse ""      of { QUALIFY(Nothing) ->
       case parse "abc"   of { QUALIFY(Nothing) ->
       case parse "0"     of { QUALIFY(Just) 0  ->
       case parse "1"     of { QUALIFY(Just) 1  ->
       case parse "101"   of { QUALIFY(Just) 5  ->
       case parse "111"   of { QUALIFY(Just) 7  ->
       case parse "10001" of { QUALIFY(Just) 17 ->
       QUALIFY(putStrLn) "Test worked";
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }; _ -> quit }; _ -> quit };
       _ -> quit }

quit = QUALIFY(putStrLn) "Test Failed"
}
