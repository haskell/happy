%-----------------------------------------------------------------------------
Target.lhs
(c) Andy Gill, Simon Marlow 1993
%-----------------------------------------------------------------------------

> module Target (Target(..)) where

> data Target
> 	= TargetHaskell			-- functions and things
>	| TargetGhc			-- haskell + ghc extensions
> 	| TargetArrayBased		-- arrays
>  deriving Eq

#ifdef __GOFER__

> instance Eq Target where (==) = primGenericEq

#endif

