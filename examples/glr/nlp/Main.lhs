> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import English

>#include "DV_lhs"

> main 
>  = do
>	[s] <- getArgs
>	case doParse $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>						++ unlines (map show f)
>			    toDV f
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show f)
>			    toDV f 
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts
>			    toDV f 

> forest_lookup f i 
>  = fromJust $ lookup i f
