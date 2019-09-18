#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

{
import Control.Monad(when)
import System.Exit

}

%monad { QUALIFY(Maybe) } { (QUALIFY(>>=)) } { QUALIFY(return) }
%tokentype { QUALIFY(Char) }
%token
  'a' { 'a' }
  'b' { 'b' }

%name test1 test1
%name test2 test2

%%

test1
  : sepBy('a','b')  { $1 }

test2
  : endBy('a','b')  { $1 }

many_rev1(p)
  : p               { [$1] }
  | many_rev1(p) p  { $2 : $1 }

many1(p)
  : many_rev1(p)    { QUALIFY(reverse) $1 }

many(p)
  : many1(p)        { $1 }
  |                 { [] }

optional(p)
  : p               { QUALIFY(Just) $1 }
  |                 { QUALIFY(Nothing) }

sepR(p,q)
  : p q             { $2 }

sepL(p,q)
  : p q             { $1 }

sepBy1(p,q)
  : p many(sepR(q,p)) { $1 : $2 }

sepBy(p,q)
  : sepBy1(p,q)       { $1 }
  |                   { [] }

endBy(p,q)
  : many (sepL(p,q))  { $1 }

endBy1(p,q)
  : many1 (sepL(p,q)) { $1 }

{
happyError _  = QUALIFY(Nothing)

tests         = [ test1 ""      QUALIFY(==) QUALIFY(Just) ""
                , test1 "a"     QUALIFY(==) QUALIFY(Just) "a"
                , test1 "ab"    QUALIFY(==) QUALIFY(Nothing)
                , test1 "aba"   QUALIFY(==) QUALIFY(Just) "aa"
                , test1 "abab"  QUALIFY(==) QUALIFY(Nothing)

                , test2 ""      QUALIFY(==) QUALIFY(Just) ""
                , test2 "a"     QUALIFY(==) QUALIFY(Nothing)
                , test2 "ab"    QUALIFY(==) QUALIFY(Just) "a"
                , test2 "aba"   QUALIFY(==) QUALIFY(Nothing)
                , test2 "abab"  QUALIFY(==) QUALIFY(Just) "aa"
                ]

main        = do let failed = QUALIFY(filter) (QUALIFY(not) QUALIFY(.) QUALIFY(snd)) (QUALIFY(zip) [0..] tests)
                 when (QUALIFY(not) (QUALIFY(null) failed)) QUALIFY($)
                   do QUALIFY(putStrLn) ("Failed tests: " QUALIFY(++) QUALIFY(show) (QUALIFY(map) QUALIFY(fst) failed))
                      exitFailure
                 QUALIFY(putStrLn) "Tests passed."

}
