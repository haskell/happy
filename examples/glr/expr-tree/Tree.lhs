> module Tree where

> data Tree a b
>  = Plus  a a 
>  | Times a a 
>  | Minus a a 
>  | Pars  a 
>  | Const b
>    deriving (Show)

Note: 
 + we need a construct for the location of parentheses
 + sometimes it is useful to keep this information anyway -- eg ghc's 
     implementation of customisable prec & assoc.
 + I've left Trees polymorphic in the "branch" type - this supports labelling
     the forest with Int-based trees then switching to Tree-based trees later
 + But this might require some non-Haskell-98 flags for the related class
     instances.

