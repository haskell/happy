/-----------------------------------------------------------------------------
Special processing for attribute grammars for the Mangler. We re-parse
the body of the code block and output the nasty-looking record
manipulation and let binding goop

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> {-# LANGUAGE ScopedTypeVariables #-}
> module Happy.Frontend.AttrGrammar.Mangler (rewriteAttributeGrammar) where

> import Happy.Grammar
> import Happy.Frontend.ParseMonad.Class
> import Happy.Frontend.AttrGrammar
> import Happy.Frontend.AttrGrammar.Parser
> import Happy.Frontend.Mangler.Monad
> import Data.List     ( findIndices, groupBy, intersperse, nub )

> import Data.List  ( sortBy )
> import Data.Maybe ( fromMaybe )

> import Control.Monad

> rewriteAttributeGrammar :: [Name] -> [Name] -> String -> AttributeGrammarExtras -> M (String,[Index])
> rewriteAttributeGrammar lhs nonterm_names code ag =

   first we need to parse the body of the code block

>     case runFromStartP agParser code 0 of
>        Left msg  -> do addErr ("error in attribute grammar rules: "++msg)
>                        return ("",[])
>        Right rules  ->

   now we break the rules into three lists, one for synthesized attributes,
   one for inherited attributes, and one for conditionals

>            let ( selfRules :: [AgSelfAssign]
>                  , subRules :: [AgSubAssign]
>                  , conditions :: [AgConditional]
>                  ) = partitionRules [] [] [] rules
>                attrNames = map fst $ attributes ag
>                defaultAttr = head attrNames

   now check that $i references are in range

>            in do let prods :: [Index]
>                      prods = mentionedProductions rules
>                  mapM_ checkArity prods

   and output the rules

>                  rulesStr <- formatRules arity attrNames defaultAttr
>                               allSubProductions selfRules
>                               subRules conditions

   return the munged code body and all sub-productions mentioned

>                  return (rulesStr,nub (allSubProductions++prods))


>    where arity :: Index 
>          arity = length lhs

>          partitionRules a b c [] = (a,b,c)
>          partitionRules a b c (RightmostAssign attr toks : xs) = partitionRules a (x:b) c xs
>             where x = MkAgSubAssign (arity,attr) toks
>          partitionRules a b c (SelfAssign x  : xs) = partitionRules (x:a) b c xs
>          partitionRules a b c (SubAssign x   : xs) = partitionRules a (x:b) c xs
>          partitionRules a b c (Conditional x : xs) = partitionRules a b (x:c) xs

>          allSubProductions             = map (+1) (findIndices (`elem` nonterm_names) lhs)

>          mentionedProductions rules    = [ i | (AgTok_SubRef (i,_)) <- concat (map getTokens rules) ]

>          getTokens (SelfAssign (MkAgSelfAssign _ toks))  = toks
>          getTokens (SubAssign (MkAgSubAssign _ toks))    = toks
>          getTokens (Conditional (MkAgConditional toks))  = toks
>          getTokens (RightmostAssign _ toks)           = toks
>
>          checkArity x = when (x > arity) $ addErr (show x++" out of range")


------------------------------------------------------------------------------------
-- Actually emit the code for the record bindings and conditionals
--

> formatRules :: Index -> [String] -> String -> [Index]
>             -> [AgSelfAssign] -> [AgSubAssign] -> [AgConditional]
>             -> M String

> formatRules arity _attrNames defaultAttr prods selfRules subRules conditions = return $
>     concat [ "\\happyInhAttrs -> let { "
>            , "happySelfAttrs = happyInhAttrs",formattedSelfRules
>            , subProductionRules
>            , "; happyConditions = ", formattedConditions
>            , " } in (happyConditions,happySelfAttrs)"
>            ]
>
>  where formattedSelfRules = case selfRules of [] -> []; _ -> "{ "++formattedSelfRules'++" }"
>        formattedSelfRules' = concat $ intersperse ", " $ map formatSelfRule selfRules
>        formatSelfRule (MkAgSelfAssign [] toks)   = defaultAttr++" = "++(formatTokens toks)
>        formatSelfRule (MkAgSelfAssign attr toks) = attr++" = "++(formatTokens toks)

>        subRulesMap :: [(Int,[(String,[AgToken])])]
>        subRulesMap = map     (\l   -> foldr (\ (_,x) (i,xs) -> (i,x:xs))
>                                             (fst $ head l,[snd $ head l])
>                                             (tail l) ) .
>                      groupBy (\x y -> (fst x) == (fst y)) .
>                      sortBy  (\x y -> compare (fst x) (fst y)) .
>                      map     (\(MkAgSubAssign (i,ident) toks) -> (i,(ident,toks))) $ subRules

>        subProductionRules = concat $ map formatSubRules prods

>        formatSubRules i =
>           let attrs = fromMaybe [] . lookup i $ subRulesMap
>               attrUpdates' = concat $ intersperse ", " $ map (formatSubRule i) attrs
>               attrUpdates  = case attrUpdates' of [] -> []; x -> "{ "++x++" }"
>           in concat ["; (happyConditions_",show i,",happySubAttrs_",show i,") = ",mkHappyVar i
>                     ," happyEmptyAttrs"
>                     , attrUpdates
>                     ]
>
>        formattedConditions = concat $ intersperse " Prelude.++ " $ localConditions : (map (\i -> "happyConditions_"++(show i)) prods)
>        localConditions = "["++(concat $ intersperse ", " $ map formatCondition conditions)++"]"
>        formatCondition (MkAgConditional toks) = formatTokens toks

>        formatSubRule _ ([],toks)   = defaultAttr++" = "++(formatTokens toks)
>        formatSubRule _ (attr,toks) = attr++" = "++(formatTokens toks)

>        formatTokens tokens = concat (map formatToken tokens)

>        formatToken AgTok_LBrace           =  "{ "
>        formatToken AgTok_RBrace           = "} "
>        formatToken AgTok_Where            = "where "
>        formatToken AgTok_Semicolon        = "; "
>        formatToken AgTok_Eq               = "="
>        formatToken (AgTok_SelfRef [])     = "("++defaultAttr++" happySelfAttrs) "
>        formatToken (AgTok_SelfRef x)      = "("++x++" happySelfAttrs) "
>        formatToken (AgTok_RightmostRef x) = formatToken (AgTok_SubRef (arity,x))
>        formatToken (AgTok_SubRef (i,[]))
>            | i `elem` prods = "("++defaultAttr++" happySubAttrs_"++(show i)++") "
>            | otherwise      = mkHappyVar i ++ " "
>        formatToken (AgTok_SubRef (i,x))
>            | i `elem` prods = "("++x++" happySubAttrs_"++(show i)++") "
>            | otherwise      = error ("lhs "++(show i)++" is not a non-terminal")
>        formatToken (AgTok_Unknown x)     = x++" "
>        formatToken AgTok_EOF = error "formatToken AgTok_EOF"

> mkHappyVar :: Int -> String
> mkHappyVar n  = "happy_var_" ++ show n
