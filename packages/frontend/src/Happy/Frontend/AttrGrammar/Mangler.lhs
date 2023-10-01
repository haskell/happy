/-----------------------------------------------------------------------------
Special processing for attribute grammars for the Mangler. We re-parse
the body of the code block and output the nasty-looking record
manipulation and let binding goop

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

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

> rewriteAttributeGrammar :: Int -> [Name] -> [Name] -> String -> [(String,String)] -> M (String,[Int])
> rewriteAttributeGrammar arity lhs nonterm_names code attrs =

   first we need to parse the body of the code block

>     case runFromStartP agParser code 0 of
>        Left msg  -> do addErr ("error in attribute grammar rules: "++msg)
>                        return ("",[])
>        Right rules  ->

   now we break the rules into three lists, one for synthesized attributes,
   one for inherited attributes, and one for conditionals

>            let (selfRules,subRules,conditions) = partitionRules [] [] [] rules
>                attrNames = map fst attrs
>                defaultAttr = head attrNames

   now check that $i references are in range

>            in do let prods = mentionedProductions rules
>                  mapM_ checkArity prods

   and output the rules

>                  rulesStr <- formatRules arity attrNames defaultAttr
>                               allSubProductions selfRules
>                               subRules conditions

   return the munged code body and all sub-productions mentioned

>                  return (rulesStr,nub (allSubProductions++prods))


>    where partitionRules a b c [] = (a,b,c)
>          partitionRules a b c (RightmostAssign attr toks : xs) = partitionRules a (SubAssign (arity,attr) toks : b) c xs
>          partitionRules a b c (x@(SelfAssign _ _ )  : xs) = partitionRules (x:a) b c xs
>          partitionRules a b c (x@(SubAssign _ _)    : xs) = partitionRules a (x:b) c xs
>          partitionRules a b c (x@(Conditional _)    : xs) = partitionRules a b (x:c) xs

>          allSubProductions             = map (+1) (findIndices (`elem` nonterm_names) lhs)

>          mentionedProductions rules    = [ i | (AgTok_SubRef (i,_)) <- concat (map getTokens rules) ]

>          getTokens (SelfAssign _ toks)      = toks
>          getTokens (SubAssign _ toks)       = toks
>          getTokens (Conditional toks)       = toks
>          getTokens (RightmostAssign _ toks) = toks
>
>          checkArity x = when (x > arity) $ addErr (show x++" out of range")


------------------------------------------------------------------------------------
-- Actually emit the code for the record bindings and conditionals
--

> formatRules :: Int -> [String] -> String -> [Name]
>             -> [AgRule] -> [AgRule] -> [AgRule]
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
>        formatSelfRule (SelfAssign [] toks)   = defaultAttr++" = "++(formatTokens toks)
>        formatSelfRule (SelfAssign attr toks) = attr++" = "++(formatTokens toks)
>        formatSelfRule _ = error "formatSelfRule: Not a self rule"

>        subRulesMap :: [(Int,[(String,[AgToken])])]
>        subRulesMap = map     (\l   -> foldr (\ (_,x) (i,xs) -> (i,x:xs))
>                                             (fst $ head l,[snd $ head l])
>                                             (tail l) ) .
>                      groupBy (\x y -> (fst x) == (fst y)) .
>                      sortBy  (\x y -> compare (fst x) (fst y)) .
>                      map     (\(SubAssign (i,ident) toks) -> (i,(ident,toks))) $ subRules

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
>        formatCondition (Conditional toks) = formatTokens toks
>        formatCondition _ = error "formatCondition: Not a condition"

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
