> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import English

> main 
>  = do
>	[s] <- getArgs
>	test s

> test s 
>  = do 
>	case doParse $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>						++ unlines (map show f)
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show f)
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts

> forest_lookup f i 
>  = case lookup i f of
>	Just (FNode _ _ s bs) -> (s,bs)
