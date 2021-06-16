-----------------------------------------------------------------------------
The target data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Backend.Target (Target(..)) where

> data Target
>       = TargetHaskell                 -- functions and things
>       | TargetArrayBased              -- arrays

>  deriving Eq
