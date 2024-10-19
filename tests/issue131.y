{
module Main where
import Prelude ()
import qualified Prelude
}

%name parser
%token foo { 1 }
%tokentype { Prelude.Int }

%%

Foo : foo { () }

{
main = Prelude.putStrLn "Test works"
happyError = Prelude.undefined
}
