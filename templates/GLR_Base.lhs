{- GLR_Base.lhs 
   $Id: GLR_Base.lhs,v 1.3 2004/10/28 12:48:49 paulcc Exp $
-} 

Basic defs required for compiling the data portion of the parser

---
We're creating Int-indexed graphs

>type ForestId  = (Int,Int,GSymbol)


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

>instance Eq Branch where
>	b1 == b2 = b_nodes b1 == b_nodes b2



%-------------------------------------------------------------------------------
Utilities for decoding 

--- 
Tree decode unpacks the forest into a list of results
 - this is ok for small examples, but inefficient for very large examples
 - the data file contains further instances
 - see documentation for further information
 - "Decode_Result" is a synonym used to insert the monad type constr (or not)

>class TreeDecode a where
>	decode_b :: (ForestId -> [Branch]) -> Branch -> [Decode_Result a]

>decode :: TreeDecode a => (ForestId -> [Branch]) -> ForestId -> [Decode_Result a]
>decode f i@(_,_,HappyTok t) 
>  = decode_b f (Branch (SemTok t) [])
>decode f i
>  = [ d | b <- f i, d <- decode_b f b ]

>instance TreeDecode Token where
>	decode_b f (Branch (SemTok t) []) = [happy_return t]

---
this is used to multiply the ambiguous possibilities from children

>--cross_fn :: [a -> b] -> [a] -> [b]
>--actual type will depend on monad in use. 
>--happy_ap defined by parser generator
>cross_fn fs as = [ f `happy_ap` a | f <- fs, a <- as]

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


