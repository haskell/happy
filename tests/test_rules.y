{
import Control.Monad(when)
import System.Exit
}

%monad { Maybe } { (>>=) } { return }
%tokentype { Char }
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
  : many_rev1(p)    { reverse $1 }

many(p)
  : many1(p)        { $1 }
  |                 { [] }

optional(p)
  : p               { Just $1 }
  |                 { Nothing }

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
happyError _  = Nothing

tests         = [ test1 ""      == Just ""
                , test1 "a"     == Just "a"
                , test1 "ab"    == Nothing
                , test1 "aba"   == Just "aa"
                , test1 "abab"  == Nothing

                , test2 ""      == Just ""
                , test2 "a"     == Nothing
                , test2 "ab"    == Just "a"
                , test2 "aba"   == Nothing
                , test2 "abab"  == Just "aa"
                ]

main        = do let failed = filter (not . snd) (zip [0..] tests)
                 when (not (null failed)) $
                   do putStrLn ("Failed tests: " ++ show (map fst failed))
                      exitFailure
                 putStrLn "Tests passed."

}
