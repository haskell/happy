> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import Expr


> main 
>  = do
>	[s] <- getArgs
>	test s

> test s 
>  = do
>	case doParse $ map (:[]) $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>						++ unlines (map show f)
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show f)
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts
>			    putStrLn $ "Error: " ++ unlines (map show f)

> forest_lookup f i@(_,_,s)
>  = fromJust $ lookup i f 
