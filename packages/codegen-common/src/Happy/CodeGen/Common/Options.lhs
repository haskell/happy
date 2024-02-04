/-----------------------------------------------------------------------------
The CommonOptions data type.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.CodeGen.Common.Options (
>       ErrorHandlerInfo(..), CommonOptions(..)
>       ) where

> data ErrorHandlerInfo
>   = DefaultErrorHandler
>   -- ^ Default handler `happyError`.
>   | CustomErrorHandler String
>   -- ^ Call this handler on error.
>   | ResumptiveErrorHandler String {- abort -} String {- addMessage -}
>   -- ^ `ResumptiveErrorHandler abort reportError`:
>   --   Calls non-fatal `reportError ... resume` with resumption `resume` to
>   --   get more errors, ultimately failing with `abort` when parse can't be
>   --   resumed.
>
> data CommonOptions
>       = CommonOptions {
>               token_type        :: String,
>               imported_identity :: Bool,
>               monad             :: (Bool,String,String,String,String),
>               expect            :: Maybe Int,
>               lexer             :: Maybe (String,String),
>               error_handler     :: ErrorHandlerInfo,
>               error_expected    :: Bool
>                 -- ^ Error handler expects a `[String]` as arg after current
>                 -- token carrying the pretty-printed expected tokens.
>       }
