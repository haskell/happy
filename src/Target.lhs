-----------------------------------------------------------------------------
$Id: Target.lhs,v 1.5 2001/04/27 10:10:23 simonmar Exp $

The target data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Target (Target(..)) where

> data Target
> 	= TargetHaskell			-- functions and things
> 	| TargetArrayBased		-- arrays

>  deriving Eq
