-----------------------------------------------------------------------------
$Id: Target.lhs,v 1.2 1997/03/27 14:14:51 simonm Exp $

The target data type.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Target (Target(..)) where

> data Target
> 	= TargetHaskell			-- functions and things
>	| TargetGhc			-- haskell + ghc extensions
> 	| TargetArrayBased		-- arrays
>  deriving Eq

