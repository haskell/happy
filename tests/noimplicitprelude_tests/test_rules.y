{
import Control.Monad(when)
import System.Exit

{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
}

%monad { Prelude.Maybe } { (Prelude.>>=) } { Prelude.return }
%tokentype { Prelude.Char }
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
  : many_rev1(p)    { Prelude.reverse $1 }

many(p)
  : many1(p)        { $1 }
  |                 { [] }

optional(p)
  : p               { Prelude.Just $1 }
  |                 { Prelude.Nothing }

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
happyError _  = Prelude.Nothing

tests         = [ test1 ""      Prelude.== Prelude.Just ""
                , test1 "a"     Prelude.== Prelude.Just "a"
                , test1 "ab"    Prelude.== Prelude.Nothing
                , test1 "aba"   Prelude.== Prelude.Just "aa"
                , test1 "abab"  Prelude.== Prelude.Nothing

                , test2 ""      Prelude.== Prelude.Just ""
                , test2 "a"     Prelude.== Prelude.Nothing
                , test2 "ab"    Prelude.== Prelude.Just "a"
                , test2 "aba"   Prelude.== Prelude.Nothing
                , test2 "abab"  Prelude.== Prelude.Just "aa"
                ]

main        = do let failed = Prelude.filter (Prelude.not Prelude.. Prelude.snd) (Prelude.zip [0..] tests)
                 when (Prelude.not (Prelude.null failed)) Prelude.$
                   do Prelude.putStrLn ("Failed tests: " Prelude.++ Prelude.show (Prelude.map Prelude.fst failed))
                      exitFailure
                 Prelude.putStrLn "Tests passed."

}
