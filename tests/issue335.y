%name foo
%tokentype { String }

%%

foo : foo { void <$> putStr "The '<$>' shall mean 'fmap' but happy will read it as '< $>' which is not what we want." }
