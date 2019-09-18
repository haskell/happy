#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

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
main =  do Exception.try (QUALIFY(print) test1 QUALIFY(>>) QUALIFY(fail) "Test failed.") :: QUALIFY(IO) (QUALIFY(Either) ErrorCall ())
           QUALIFY(putStrLn) "Test worked"

happyError = QUALIFY(error) "parse error"
}
