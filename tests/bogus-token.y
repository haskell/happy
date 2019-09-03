#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
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
main =  do Exception.try (QUALIFIEDPRELUDE.print test1 QUALIFIEDPRELUDE.>> QUALIFIEDPRELUDE.fail "Test failed.") :: QUALIFIEDPRELUDE.IO (QUALIFIEDPRELUDE.Either ErrorCall ())
           QUALIFIEDPRELUDE.putStrLn "Test worked"

happyError = QUALIFIEDPRELUDE.error "parse error"
}
