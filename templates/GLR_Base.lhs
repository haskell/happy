{- GLR_Base.lhs 
   $Id: GLR_Base.lhs,v 1.1 2004/08/11 15:39:32 paulcc Exp $
-} 

Basic defs required for compiling the data portion of the parser

---
We're creating Int-indexed graphs

>type ForestId  = Int


---
Actions for the GLR machine

>data GLRAction = Shift Int [Reduction]
>               | Reduce [Reduction]
>               | Accept
>               | Error 

---
A Reduction (s,n,f) removes the top n node-ids, creates a new branch from these
and labels the branch with the given symbol s. Additionally, the branch may
hold some semantic value. 

>type Reduction = (GSymbol,Int, [ForestId] -> Branch)


---
A Branch holds the semantic result plus node ids of children

>data Branch
> = Branch {b_sem :: GSem, b_nodes :: [ForestId]}
>   deriving Show




%-------------------------------------------------------------------------------
Utilities for decoding 

--- 
Tree decode unpacks the forest into a list of results
 - this is ok for small examples, but inefficient for very large examples
 - the data file contains further instances
 - see documentation for further information

>class TreeDecode a where
>	decode_b :: (ForestId -> (GSymbol,[Branch])) -> Branch -> [a]

>decode :: TreeDecode a => (ForestId -> (GSymbol,[Branch])) -> Int -> [a]
>decode f i = case f i of 
>               (HappyTok t, []) -> decode_b f (Branch (SemTok t) [])
>               (s,          bs) -> [ d | b <- bs, d <- decode_b f b ]

>instance TreeDecode Token where
>	decode_b f (Branch (SemTok t) []) = [t]

---
this is used to multiply the ambiguous possibilities from children

>cross_fn :: [a -> b] -> [a] -> [b]
>cross_fn fs as = [ f a | f <- fs, a <- as]

---
Label decoding unpacks from the Semantic wrapper type
 - this allows arbitrary values (within the limits of the compiler settings)
   to be recovered from nodes in the tree.
 - again, more instances are written in the data file
 - see documentation for further information

>class LabelDecode a where
>	unpack :: GSem -> a
>instance LabelDecode Token where
>	unpack (SemTok t) = t


