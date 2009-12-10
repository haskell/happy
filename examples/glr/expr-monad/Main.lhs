> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import qualified Data.Map as Map
> import Expr

#include "DV_lhs"

This requires CPP / preprocessing; use Hugs.lhs for tests with Hugs

> main 
>  = do
>	[s] <- getArgs
>	case doParse $ map (:[]) $ lexer s of 
>	  ParseOK r f -> do 
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>						++ unlines (map show $ Map.toList f)
>			    let ms = decode (forest_lookup f) r ::[IO Int]
>			    mapM_ (\ma -> catch ma (\_ -> return 0) >>= print) ms
>			    toDV $ Map.toList f
>	  ParseEOF f  -> do 
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show $ Map.toList f)
>			    toDV $ Map.toList f 
>	  ParseError ts f -> do 
>			    putStrLn $ "Error: " ++ show ts
>			    toDV $ Map.toList f 

> forest_lookup f i 
>  = fromJust $ Map.lookup f i
