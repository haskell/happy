
module ParserM (
    -- Parser Monad
    ParserM(..), AlexInput, run_parser,
    -- Parser state
    St, StartCode, start_code, set_start_code,
    -- Tokens
    Token(..),
    -- Tree
    Tree(..),
    -- Actions
    Action, andBegin, mkT,
    -- Positions
    get_pos, show_pos,
    -- Input
    alexGetByte, alexInputPrevChar, input, position,
    -- Other
    happyError
 ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Char (ord)
import Data.Word (Word8)

-- Parser Monad
newtype ParserM a = ParserM (AlexInput -> StateT St (Either String) (AlexInput, a))

instance Functor ParserM where
    fmap = liftM

instance Applicative ParserM where
    pure a = ParserM $ \i -> return (i, a)
    (<*>) = ap

instance Monad ParserM where
    return = pure
    ParserM m >>= k = ParserM $ \i -> do (i', x) <- m i
                                         case k x of
                                             ParserM y -> y i'
    fail err = ParserM $ \_ -> fail err

run_parser :: ParserM a -> (String -> Either String a)
run_parser (ParserM p)
 = \s -> case evalStateT (p (AlexInput init_pos s)) init_state of
             Left es -> throwError es
             Right (_, x) -> return x

-- Parser state

data St = St {start_code :: !StartCode}
type StartCode = Int

init_state :: St
init_state = St 0

-- Tokens

data Token = TEOF
           | TFork
           | TLeaf

-- Tree

data Tree = Leaf
          | Fork Tree Tree
    deriving Show

-- Actions

type Action = (AlexInput, String) -> StateT St (Either String) (Token, AlexInput)

set_start_code :: StartCode -> StateT St (Either String) ()
set_start_code sc = do st <- get
                       put $ st { start_code = sc }

andBegin :: Action -> StartCode -> Action
(act `andBegin` sc) x = do set_start_code sc
                           act x

mkT :: Token -> Action
mkT t (p,_) = lift $ return (t, p)

-- Positions

data Pos = Pos !Int{- Line -} !Int{- Column -}

get_pos :: ParserM Pos
get_pos = ParserM $ \i@(AlexInput p _) -> return (i, p)

alexMove :: Pos -> Char -> Pos
alexMove (Pos l _) '\n' = Pos (l+1) 1
alexMove (Pos l c) '\t' = Pos l ((c+8) `div` 8 * 8)
alexMove (Pos l c) _    = Pos l (c+1)

init_pos :: Pos
init_pos = Pos 1 1

show_pos :: Pos -> String
show_pos (Pos l c) = "line " ++ show l ++ ", column " ++ show c

-- Input

data AlexInput = AlexInput {position :: !Pos, input :: String}

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput p (x:xs)) = Just (fromIntegral (ord x),
                                         AlexInput (alexMove p x) xs)
alexGetByte (AlexInput _ []) = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

happyError :: ParserM a
happyError = do p <- get_pos
                fail $ "Parse error at " ++ show_pos p

