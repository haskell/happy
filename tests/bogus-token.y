{
module Main where
import Exception
}

%tokentype { Token }
%token A { A }

%name parse

%%

parse : A { () }

{
data Token = A | B

test1 = parse [B]
main =  do Exception.tryJust errorCalls (print test1 >> fail "Test failed.")
           putStrLn "Test worked"

happyError = error "parse error"
}
