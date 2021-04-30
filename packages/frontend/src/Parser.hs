module Parser (module X) where

-- We use the bootstrapped version if it is available
#ifdef HAPPY_BOOTSTRAP
import Parser.Bootstrapped as X
#else
import Parser.Oracle as X
#endif
