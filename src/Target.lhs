-----------------------------------------------------------------------------
$Id: Target.lhs,v 1.4 2000/12/03 16:53:53 simonmar Exp $

The target data type.

(c) 1993-2000 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Target (Target(..)) where

> data Target
> 	= TargetHaskell			-- functions and things
> 	| TargetArrayBased		-- arrays

>  deriving Eq
