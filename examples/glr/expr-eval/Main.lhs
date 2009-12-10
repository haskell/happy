> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import qualified Data.Map as Map
> import Expr

#include "DV_lhs"

This requires CPP / preprocessing; use Hugs.lhs for tests with Hugs

> main 
>  = do
>	(s:o) <- getArgs
>	let x = concat o
>	case doParse $ map (:[]) $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>                               ++ (if 'f' `elem` x then unlines (map show $ Map.toList f) else "")
>                               ++ (if 'r' `elem` x then unlines (map show (decode (forest_lookup f) r ::[Int])) else "")
>			    if 'g' `elem` x then toDV (Map.toList f) else return ()
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show $ Map.toList f)
>			    toDV $ Map.toList f 
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts
>			    toDV $ Map.toList f 

> forest_lookup f i 
>  = fromJust $ Map.lookup f i
