>
>
> type HappyLexObj = Char -> (Bool,HappyLex)
> data HappyLex = NoMoreGuv | HappyLexMore HappyLexObj


> build_example "" = NoMoreGuv
> build_example [c]
>	= HappyLexMore (\ c' ->	if c == c' 
>				then (True,NoMoreGuv)
>				else (False,NoMoreGuv))
> build_example (c:cs) 
>	= HappyLexMore (\ c' ->	if c == c' 
>				then (False,build_example cs)
>				else (False,NoMoreGuv))


> build_example' "" = NoMoreGuv
> build_example' [c]
>	= HappyLexMore (\ c' ->	if c == c' 
>				then (True,NoMoreGuv)
>				else (False,NoMoreGuv))
> build_example' (c:cs) 
>	= HappyLexMore (\ c' ->	if c == c' 
>				then (True,build_example cs)
>				else (False,NoMoreGuv))



(Int {- match number -},Bool {- if you are matching -},HappyLex {- how you read more)

> evalLex :: [(Int,Bool,HappyLex)] -> Char -> [(Int,Bool,HappyLex)]
> evalLex [] _ = []
> evalLex ((i,b,NoMoreGuv):rest) c = evalLex rest c
> evalLex ((i,b,HappyLexMore fn):rest) c 
>	= case fn c of
>	    (b',more) -> (i,b',more) : evalLex rest c

> eliminate_old_matches :: [(Int,Bool,HappyLex)] -> [(Int,Bool,HappyLex)]
> eliminate_old_matches 
>	 = filter (\(i,b,lex) -> case lex of
>				   NoMoreGuv -> False
>				   _ -> True)	  						

 happyGenericLexer :: [(Int,Bool,HappyLex)] -> String -> String -> [(Int,String)]

> happyGenericLexer toks sofar [] = (0,'*':sofar,toks)
> happyGenericLexer toks sofar (c:cs) =
>   let
>     toks' = evalLex toks c
>     toks'' = eliminate_old_matches toks'
>   in
>     if null toks'' 
>     then case lookforGoodTok toks' of
>	    [] -> (0,"fdgdfg",toks)
>	    [n] -> (n,reverse (c:sofar),toks')
>     else happyGenericLexer toks'' (c:sofar) cs

> lookforGoodTok [] = []
> lookforGoodTok ((n,True,_):r) = [n]
> lookforGoodTok (_:r) = lookforGoodTok r



> test = [mark 1 (build_example "foo"),
>	  mark 2 (build_example' "fooo")]

> mark n a = (n,False,a)
