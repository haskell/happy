-----------------------------------------------------------------------------
$Id: Target.lhs,v 1.3 1999/10/07 15:17:40 simonmar Exp $

The target data type.

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Target (Target(..)) where

> data Target
> 	= TargetHaskell			-- functions and things
> 	| TargetArrayBased		-- arrays

>  deriving Eq
