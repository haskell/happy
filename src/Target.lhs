-----------------------------------------------------------------------------
$Id: Target.lhs,v 1.1 1997/02/11 13:12:09 simonm Exp $

The target data type.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Target (Target(..)) where

> data Target
> 	= TargetHaskell			-- functions and things
>	| TargetGhc			-- haskell + ghc extensions
> 	| TargetArrayBased		-- arrays
>  deriving Eq

