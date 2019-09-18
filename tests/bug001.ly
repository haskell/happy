#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

> {
>
> }

> %name parse
> %tokentype { Token }
> %token Int { TokenInt }
> %%

> Expr :: { QUALIFY(Int) }
> Expr : Term { $1 }

The constant in the next rule would be defaulted to Integer, but it is
forced to Int by the type signature of Expr above.  This test exposed
a bug in the unsafeCoerce method.

> Term : Int { 42 }

> {
> main = QUALIFY(print) (parse [TokenInt])
>
> data Token = TokenInt
>
> happyError = QUALIFY(error) ""
> }
