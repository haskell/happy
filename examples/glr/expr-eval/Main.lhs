> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import FiniteMap(fmToList,lookupFM)
> import Expr

>#include "DV_lhs"

This requires CPP / preprocessing; use Hugs.lhs for tests with Hugs

> main 
>  = do
>	[s] <- getArgs
>	case doParse $ map (:[]) $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>						++ unlines (map show $ fmToList f)
>			    putStrLn $ show (decode (forest_lookup f) r ::[Int])
>			    toDV $ fmToList f
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show $ fmToList f)
>			    toDV $ fmToList f 
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts
>			    toDV $ fmToList f 

> forest_lookup f i 
>  = fromJust $ lookupFM f i
