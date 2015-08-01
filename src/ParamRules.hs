module ParamRules(expand_rules) where

import AbsSyn
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Instances()  -- mtl is broken, so we use Either monad
import Data.List(partition,intersperse)
import qualified Data.Set as S
import qualified Data.Map as M    -- XXX: Make it work with old GHC.

expand_rules :: [Rule] -> Either String [Rule1]
expand_rules rs = do let (funs,rs1) = split_rules rs
                     (as,is) <- runM2 (mapM (`inst_rule` []) rs1)
                     bs <- make_insts funs (S.toList is) S.empty
                     return (as++bs)

type RuleName = String
type Inst     = (RuleName, [RuleName])
type Funs     = M.Map RuleName Rule
type Rule1    = (RuleName,[Prod1],Maybe (String, Subst))
type Prod1    = ([RuleName],String,Int,Maybe String)

inst_name :: Inst -> RuleName
inst_name (f,[])  = f
--inst_name (f,xs)  = f ++ "(" ++ concat (intersperse "," xs) ++ ")"
inst_name (f,xs)  = f ++ "__" ++ concat (intersperse "__" xs) ++ "__"


-- | A renaming substitution used when we instantiate a parameterized rule.
type Subst    = [(RuleName,RuleName)]
type M1       = Writer (S.Set Inst)
type M2       = ErrorT String M1

-- | Collects the instances arising from a term.
from_term :: Subst -> Term -> M1 RuleName
from_term s (App f [])  = return $ case lookup f s of
                            Just g  -> g
                            Nothing -> f

from_term s (App f ts)  = do xs <- from_terms s ts
                             let i = (f,xs)
                             tell (S.singleton i)
                             return $ inst_name i

-- | Collects the instances arising from a list of terms.
from_terms :: Subst -> [Term] -> M1 [RuleName]
from_terms s ts = mapM (from_term s) ts

-- XXX: perhaps change the line to the line of the instance
inst_prod :: Subst -> Prod -> M1 Prod1
inst_prod s (ts,c,l,p)  = do xs <- from_terms s ts
                             return (xs,c,l,p)

inst_rule :: Rule -> [RuleName] -> M2 Rule1
inst_rule (x,xs,ps,t) ts  = do s <- build xs ts []
                               ps1 <- lift $ mapM (inst_prod s) ps
                               let y = inst_name (x,ts)
                               return (y,ps1,fmap (\x -> (x,s)) t)
  where build (x':xs') (t':ts') m = build xs' ts' ((x',t'):m)
        build [] [] m  = return m
        build xs' [] _  = err ("Need " ++ show (length xs') ++ " more arguments")
        build _ ts' _   = err (show (length ts') ++ " arguments too many.")

        err m = throwError ("In " ++ inst_name (x,ts) ++ ": " ++ m)

make_rule :: Funs -> Inst -> M2 Rule1
make_rule funs (f,xs) =
  case M.lookup f funs of
    Just r  -> inst_rule r xs
    Nothing -> throwError ("Undefined rule: " ++ f)

runM2 :: ErrorT e (Writer w) a -> Either e (a, w)
runM2 m = case runWriter (runErrorT m) of
            (Left e,_)   -> Left e
            (Right a,xs) -> Right (a,xs)

make_insts :: Funs -> [Inst] -> S.Set Inst -> Either String [Rule1]
make_insts _ [] _ = return []
make_insts funs is done =
  do (as,ws) <- runM2 (mapM (make_rule funs) is)
     let done1 = S.union (S.fromList is) done
     let is1 = filter (not . (`S.member` done1)) (S.toList ws)
     bs <- make_insts funs is1 done1
     return (as++bs)


split_rules :: [Rule] -> (Funs,[Rule])
split_rules rs = let (xs,ys) = partition has_args rs
                 in (M.fromList [ (x,r) | r@(x,_,_,_) <- xs ],ys)
  where has_args (_,xs,_,_) = not (null xs)



