module Happy.Frontend.ParseMonad (module X) where

-- We use the bootstrapped version if it is available
#ifdef HAPPY_BOOTSTRAP
import Happy.Frontend.ParseMonad.Bootstrapped as X
#else
import Happy.Frontend.ParseMonad.Oracle as X
#endif
