#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

> {
>
> }

> %name parse
> %tokentype { Token }
> %token Int { TokenInt }
> %%

> Expr :: { QUALIFIEDPRELUDE.Int }
> Expr : Term { $1 }

The constant in the next rule would be defaulted to Integer, but it is
forced to Int by the type signature of Expr above.  This test exposed
a bug in the unsafeCoerce method.

> Term : Int { 42 }

> {
> main = QUALIFIEDPRELUDE.print (parse [TokenInt])
>
> data Token = TokenInt
>
> happyError = QUALIFIEDPRELUDE.error ""
> }
