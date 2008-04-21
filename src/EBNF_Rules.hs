module EBNF_Rules(ebnf_rules) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Set as S

ebnf_rules :: [Rule] -> [Rule]
ebnf_rules rs = new_rules ++ rs'
  where (rs',todo)  = runWriter (mapM check_rule rs)
        new_rules   = flip evalState S.empty
                    $ execWriterT
                    $ mapM_ make_rule (S.toAscList todo)


data EBNF_Rule_Type = Opt | Many | Many1 deriving (Show,Eq,Ord)

type ToDo = (EBNF_Rule_Type,String)
type M1   = Writer (S.Set ToDo)
            -- The set of rules that we need to generate.

-- XXX: Perhaps use Data.Sequence insted of list?
type M2   = WriterT [Rule] (State (S.Set String))
            -- Writer: list of generated rules.
            -- State: non-terminals for which we have generated
            -- a "many1 reversed" rules.  This might be a bit of an overkill.

-- XXX: Should be in AbsSyn.
type Rule = (String,[Prod],Maybe String)
type Prod = ([String], String, Int, Maybe String)

check_rule               :: Rule -> M1 Rule
check_rule (x,ps,t)       = do ps' <- mapM check_pred ps
                               return (x,ps',t)

check_pred               :: Prod -> M1 Prod
check_pred (def,x,y,z)    = do def' <- check_def def
                               return (def',x,y,z)

check_def                :: [String] -> M1 [String]
check_def (x : p : xs)    = case known x p of
                              Just r -> do tell (S.singleton r)
                                           ys <- check_def xs
                                           return (ebnf_name r : ys)
                              Nothing -> do ys <- check_def (p : xs)
                                            return (x : ys)
check_def xs              = return xs



known x c = case c of
  "?" -> Just (Opt,x)
  "*" -> Just (Many,x)
  "+" -> Just (Many1,x)
  _   -> Nothing

ebnf_name (Opt,x)   = x ++ "?"
ebnf_name (Many,x)  = x ++ "*"
ebnf_name (Many1,x) = x ++ "+"

(-->) :: [String] -> String -> Prod
xs --> ys = (xs,ys,0,Nothing)

make_rule :: ToDo -> M2 ()
make_rule rule        = do prods <- make_prods rule
                           tell [(ebnf_name rule, prods, Nothing)]

make_prods :: ToDo -> M2 [ Prod ]
make_prods (Opt,x)    = return [ [x]  --> "Just $1"
                               , []   --> "Nothing"
                               ]

make_prods (Many,x)   = do r <- many1_rev x
                           return [ [r] --> "reverse $1"
                                  , []  --> "[]"
                                  ]

make_prods (Many1,x)  = do r <- many1_rev x
                           return [ [r] --> "reverse $1"
                                  ]

many1_rev :: String -> M2 String
many1_rev name = do rs <- get
                    when (not (S.member name rs)) $
                      do put (S.insert name rs)
                         tell [(this_name,prods,Nothing)]
                    return this_name
  where this_name = name ++ "+rev"
        prods     = [ [name]            --> "[$1]"
                    , [this_name, name] --> "$2 : $1"
                    ]



