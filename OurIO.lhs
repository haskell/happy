> module OurIO where

A *very* lightweight 1.3 like interface to IO.

> infixr 3 >>
> infixr 4 >>=

> type OurIO a = (a -> Dialogue) -> Dialogue
> returnOurIO a cont = cont a
> m >>= k = \ cont -> m (\ a -> k a cont)
> m >> k = m >>= \ _ -> k
> runStrCont :: (FailCont -> StrCont -> Dialogue) -> OurIO String
> runStrCont m cont = m exit cont
> runSuccCont :: (FailCont -> SuccCont -> Dialogue) -> OurIO ()
> runSuccCont m cont = m exit (cont ())
> runStrListCont :: (FailCont -> StrListCont -> Dialogue) -> OurIO [String]
> runStrListCont m cont = m exit cont

> runOurIO :: OurIO a -> Dialogue
> runOurIO io = io (\ _ _ -> [])

> printOurIO :: String -> OurIO ()
> printOurIO = runSuccCont . appendChan stdout

