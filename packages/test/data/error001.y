%name foo
%tokentype { Token }

%%

foo : 'a' { }

bar : 'a' { }

foo : 'b' { }
