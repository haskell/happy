-- Testing the %shift directive

{
module Main where

import System.IO
import Data.Char
}

%expect 0    -- We must resolve the conflicts with %shift
%name group_a
%tokentype { Token }

%token 'A' { A }

%%
exp : exp 'A'       %shift { $1 ++ ",A" }
    | exp 'A' 'A'          { $1 ++ ",2A" }
    |                      { "S" }

{
main =
  if group_a [A, A, A] == "S,2A,A"
    then return ()
    else error "bad parse"

data Token = A

happyError _ = error "parse error"
}
