module AttrGrammarParser (module X) where

-- We use the bootstrapped version if it is available
#ifdef HAPPY_BOOTSTRAP
import AttrGrammarParser.Bootstrapped as X
#else
import AttrGrammarParser.Oracle as X
#endif
