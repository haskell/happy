{
module Main where
import Control.Exception as Exception
}

%tokentype { Token }
%token A { A }

%name parse

%%

parse : A { () }

{
data Token = A | B

test1 = parse [B]
main =  do Exception.try (print test1 >> fail "Test failed.") :: IO (Either ErrorCall ())
           putStrLn "Test worked"

happyError = error "parse error"
}
