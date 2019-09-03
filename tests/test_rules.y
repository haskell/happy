#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{
import Control.Monad(when)
import System.Exit

}

%monad { QUALIFIEDPRELUDE.Maybe } { (QUALIFIEDPRELUDE.>>=) } { QUALIFIEDPRELUDE.return }
%tokentype { QUALIFIEDPRELUDE.Char }
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
  : many_rev1(p)    { QUALIFIEDPRELUDE.reverse $1 }

many(p)
  : many1(p)        { $1 }
  |                 { [] }

optional(p)
  : p               { QUALIFIEDPRELUDE.Just $1 }
  |                 { QUALIFIEDPRELUDE.Nothing }

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
happyError _  = QUALIFIEDPRELUDE.Nothing

tests         = [ test1 ""      QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Just ""
                , test1 "a"     QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Just "a"
                , test1 "ab"    QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Nothing
                , test1 "aba"   QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Just "aa"
                , test1 "abab"  QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Nothing

                , test2 ""      QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Just ""
                , test2 "a"     QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Nothing
                , test2 "ab"    QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Just "a"
                , test2 "aba"   QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Nothing
                , test2 "abab"  QUALIFIEDPRELUDE.== QUALIFIEDPRELUDE.Just "aa"
                ]

main        = do let failed = QUALIFIEDPRELUDE.filter (QUALIFIEDPRELUDE.not QUALIFIEDPRELUDE.. QUALIFIEDPRELUDE.snd) (QUALIFIEDPRELUDE.zip [0..] tests)
                 when (QUALIFIEDPRELUDE.not (QUALIFIEDPRELUDE.null failed)) QUALIFIEDPRELUDE.$
                   do QUALIFIEDPRELUDE.putStrLn ("Failed tests: " QUALIFIEDPRELUDE.++ QUALIFIEDPRELUDE.show (QUALIFIEDPRELUDE.map QUALIFIEDPRELUDE.fst failed))
                      exitFailure
                 QUALIFIEDPRELUDE.putStrLn "Tests passed."

}
