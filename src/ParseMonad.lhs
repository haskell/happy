-----------------------------------------------------------------------------
$Id: ParseMonad.lhs,v 1.2 1997/03/27 14:14:45 simonm Exp $

The parser monad.

(c) 1996 Simon Marlow
-----------------------------------------------------------------------------

> module ParseMonad where

> data ParseResult a = OkP a | FailP String
> type P a = String -> Int -> ParseResult a

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s l -> case m s l of { OkP a -> k a s l; FailP s -> FailP s }

> returnP :: a -> P a
> returnP m _ _ = OkP m

> failP :: String -> P a
> failP s _ _ = FailP s

