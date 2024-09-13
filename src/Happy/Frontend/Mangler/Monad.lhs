-----------------------------------------------------------------------------
Monad for error handling for the mangler

Pulled out so it can be shared with the attribute grammar part of the
mangler too.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Frontend.Mangler.Monad
>   ( ErrMsg
>   , M
>   , addErr
>   ) where

> import Control.Monad.Writer ( Writer, MonadWriter(..) )

> type ErrMsg = String
> type M a = Writer [ErrMsg] a

> addErr :: ErrMsg -> M ()
> addErr e = tell [e]
