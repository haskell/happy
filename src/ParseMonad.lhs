-----------------------------------------------------------------------------
The parser monad.

(c) 2001 Simon Marlow
-----------------------------------------------------------------------------

> module ParseMonad where

> data ParseResult a = OkP a | FailP String
> newtype P a = P (String -> Int -> ParseResult a)
> runP (P f) = f

> lineP :: P Int
> lineP = P $ \_ l -> OkP l

> instance Monad P where
>	return m = P $ \ _ _ -> OkP m
>	m >>= k =  P $ \s l -> case runP m s l of
>		OkP a -> runP (k a) s l
>		FailP s -> FailP s
>	fail s = P $ \ _ _ -> FailP s
