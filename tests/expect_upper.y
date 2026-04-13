-- Test that %expect checks for an upper bound, not an exact match.
-- This grammar has exactly 1 shift/reduce conflict (the dangling else).
-- %expect 2 should succeed because 1 <= 2.

{
module Main where
}

%expect 2
%name parse
%tokentype { Token }

%token
  'if'   { If }
  'then' { Then }
  'else' { Else }
  'x'    { X }

%%

stmt : 'if' 'x' 'then' stmt 'else' stmt  { $4 ++ $6 }
     | 'if' 'x' 'then' stmt               { $4 }
     | 'x'                                 { "x" }

{
main :: IO ()
main =
  if parse [If, X, Then, X] == "x"
    then return ()
    else error "bad parse"

data Token = If | Then | Else | X

happyError _ = error "parse error"
}
