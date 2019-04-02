{
module Main where

{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
}

%name parser
%token foo { 1 }
%tokentype { Prelude.Int }

%%

-- two productions for the same non-terminal should work
Foo : {- empty -} { () }
Foo : Foo foo     { () }

{
main = Prelude.return ()
happyError = Prelude.undefined
}
