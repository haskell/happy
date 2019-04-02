> {
> {-# LANGUAGE NoImplicitPrelude #-}
> import qualified Prelude
> }

> %name parse
> %tokentype { Token }
> %token Int { TokenInt }
> %%

> Expr :: { Prelude.Int }
> Expr : Term { $1 }

The constant in the next rule would be defaulted to Integer, but it is
forced to Int by the type signature of Expr above.  This test exposed
a bug in the unsafeCoerce method.

> Term : Int { 42 }

> {
> main = Prelude.print (parse [TokenInt])
>
> data Token = TokenInt
>
> happyError = Prelude.error ""
> }
