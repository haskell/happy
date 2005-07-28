> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import FiniteMap(fmToList,lookupFM)
> import Expr

>#include "DV_lhs"

This requires CPP / preprocessing; use Hugs.lhs for tests with Hugs


> main 
>  = do
>	(s:o) <- getArgs
>	let x = concat o
>	case doParse $ map (:[]) $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>                               ++ (if 'f' `elem` x then unlines (map show $ fmToList f) else "")
>			    if 'g' `elem` x then toDV (fmToList f) else return ()
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>					++ unlines (map show $ fmToList f)
>			    toDV $ fmToList f 
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts
>			    toDV $ fmToList f 

> forest_lookup f i 
>  = fromJust $ lookupFM f i
