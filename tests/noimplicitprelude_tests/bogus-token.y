{
module Main where
import Control.Exception as Exception

{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
}

%tokentype { Token }
%token A { A }

%name parse

%%

parse : A { () }

{
data Token = A | B

test1 = parse [B]
main =  do Exception.try (Prelude.print test1 Prelude.>> Prelude.fail "Test failed.") :: Prelude.IO (Prelude.Either ErrorCall ())
           Prelude.putStrLn "Test worked"

happyError = Prelude.error "parse error"
}
