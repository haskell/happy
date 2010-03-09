> module Main where
> import System(getArgs)
> import Data.Maybe(fromJust)
> import FiniteMap(fmToList,lookupFM)
> import Expr


> main 
>  = do
>	[s] <- getArgs
>	test (read s :: Int)

> test n
>  = do
>	case doParse $ map (:[]) $ lexer $ replicate n '+' of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>					++ unlines (map show $ fmToList f)
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>					++ unlines (map show $ fmToList f)
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts

> forest_lookup f i 
>  = fromJust $ lookupFM f i
