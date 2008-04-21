{
import Control.Monad(when)
import System.Exit
}

%token
  'a' { 'a' }
  'b' { 'b' }
  'c' { 'c' }

%tokentype { Char }
%monad { Maybe }  { (>>=) }{ return }
%name test1 test1
%name test2 test2
%name test3 test3
%name test4 test4

%%

test1: x?           { maybe "" return $1 }
test2: x*           { $1 }
test3: x+           { $1 }
test4: x+ 'c'+ x*   { $1 ++ $2 ++ $3 }

x : 'a'             { 'a' }
  | 'b'             { 'b' }

{
happyError _  = Nothing

accept p x    = case p x of
                  Just x1 | x == x1 -> True
                  _ -> False

reject p x    = case p x of
                  Nothing -> True
                  Just {} -> False

tests         = [ accept test1 ""
                , accept test1 "a"
                , accept test1 "b"
                , reject test1 "c"
                , reject test1 "aa"

                , accept test2 ""
                , accept test2 "aaabbbaab"
                , reject test2 "aaabccaa"

                , reject test3 ""
                , accept test3 "aaabbbaab"
                , reject test3 "aaabccaa"

                , accept test4 "aaabbaccc"
                , accept test4 "aaabbacccababab"
                , reject test4 ""
                , reject test4 "aaabbb"
                ]

main        = do let failed = filter (not . snd) (zip [0..] tests)
                 when (not (null failed)) $
                   do putStrLn ("Failed tests: " ++ show (map fst failed))
                      exitFailure
                 putStrLn "Tests passed."

}


