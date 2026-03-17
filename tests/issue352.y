{
module Issue352 where
}
%name parseS S
%tokentype { Token }
%token
  a       { A }
  w       { W }
  useless { U }

%%

UseLess : useless UseLess  { () }
        | useless          { () }

S : a w  { () }

{
data Token = A | W | U

happyError :: [Token] -> a
happyError _ = error "err"
}
