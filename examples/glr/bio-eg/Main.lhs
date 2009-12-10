> module Main where
> import System(getArgs)
> import Maybe(fromJust)
> import Bio
> import qualified Data.Map as Map
> import Control.Monad.State

#include "DV_lhs"

> main 
>  = do
>	[s] <- getArgs
>	case doParse $ map (:[]) $ lexer s of 
>	  ParseOK r f -> do 
>			    let f_ = filter_noise $ Map.toList f
>			    putStrLn $ "Ok " ++ show r ++ "\n" 
>						++ unlines (map show f_)
>			    --writeFile "full" (unlines $ map show f)
>			    toDV (trim_graph f_ r)
>	  ParseEOF f  -> do 
>			    let f_ = filter_noise $ Map.toList f
>			    putStrLn $ "Premature end of input:\n" 
>						++ unlines (map show f_)
>			    toDV f_
>			    --writeFile "full" (unlines $ map show f)
>	  ParseError ts f -> do 
>			    let f_ = filter_noise $ Map.toList f
>			    putStrLn $ "Error: " ++ show ts
>			    toDV f_ 
>			    --writeFile "full" (unlines $ map show f)

> forest_lookup f i
>  = fromJust $ Map.lookup f i

---
remove intergenic things, to make graph small enough for drawing
 -- (prefer to do this with filtering in parser...)

> filter_noise f
>  = [ (i, map filter_branch bs) 
>    | (i@(s_i,e_i,l), bs) <- f, not_igs i ]
>    where
>	igs = listToFM [ (i,False) | i@(_,_,G_Intergenic_noise) <- map fst f ]
>	not_igs = lookupWithDefaultFM igs True 
>	filter_branch (Branch s ns) = Branch s [ n | n <- ns, not_igs n ]

> trim_graph :: NodeMap -> RootNode -> NodeMap
> trim_graph f r
>  = [ (i,n) | (i,n) <- f, lookupWithDefaultFM wanted False i ]
>    where
>	table = listToFM f
>	wanted = snd $ runState (follow r) emptyFM
>	follow :: ForestId -> State (FiniteMap ForestId Bool) ()
>	follow i = do
>	             visited <- get 
>	             if lookupWithDefaultFM visited False i
>	               then return ()
>	               else do
>	                      case Map.lookup table i of 
>	                        Nothing 
>	                          -> error $ "bad node: " ++ show i
>	                        Just bs
>	                          -> do
>	                                modify (\s -> addToFM s i True)
>	                                mapM_ follow $ concatMap b_nodes bs

