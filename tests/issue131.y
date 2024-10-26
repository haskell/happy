{
module Main where
import Prelude ()
import qualified Prelude as Pre
}

%name parser
%token foo { 1 }
%tokentype { Pre.Int }

%%

Foo : foo { () }

{
main = Pre.putStrLn "Test works"
happyError = Pre.undefined
}
