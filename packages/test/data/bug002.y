{
module Main where
}

%name parser
%token foo { 1 }
%tokentype { Int }

%%

-- two productions for the same non-terminal should work
Foo : {- empty -} { () }
Foo : Foo foo     { () } 

{ 
main = return ()
happyError = undefined
}
