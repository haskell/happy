module Happy.Frontend.Parser (module X) where

-- We use the bootstrapped version if it is available
#ifdef HAPPY_BOOTSTRAP
import Happy.Frontend.Parser.Bootstrapped as X
#else
import Happy.Frontend.Parser.Oracle as X
#endif
