module ParseMonad (module X) where

-- We use the bootstrapped version if it is available
#ifdef HAPPY_BOOTSTRAP
import ParseMonad.Bootstrapped as X
#else
import ParseMonad.Oracle as X
#endif
