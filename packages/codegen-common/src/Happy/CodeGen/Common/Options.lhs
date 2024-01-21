/-----------------------------------------------------------------------------
The CommonOptions data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.CodeGen.Common.Options (
>       ErrorHandlerType(..),
>       CommonOptions(..)
>       ) where

> data ErrorHandlerType
>   = ErrorHandlerTypeDefault
>   | ErrorHandlerTypeExpList

> data CommonOptions
>       = CommonOptions {
>               token_type        :: String,
>               imported_identity :: Bool,
>               monad             :: (Bool,String,String,String,String),
>               expect            :: Maybe Int,
>               lexer             :: Maybe (String,String),
>               error_handler     :: Maybe String,
>               error_sig         :: ErrorHandlerType,
>                 -- ^ ErrorHandlerTypExpList: error handler expects a
>                 --   `[String]` as first arg with the pretty-printed expected
>                 --   tokens
>               error_resumptive  :: Bool
>                 -- ^ `True` => The error handler expects a `resume`
>                 -- continuation as last argument.
>       }
