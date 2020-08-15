{-# LANGUAGE RankNTypes #-}

module ParseMonad where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad

type Pfunc a = String -> Int -> ParseResult a

type ParseResult = Either String

data PState token =
  PS !String
     !Int
     !(Maybe token)

class HasLexer token where
  lexToken :: (token -> Pfunc r) -> Pfunc r

data Decision token = Consume | PutBack token

type P' token a =
  forall r. (a -> PState token -> ParseResult r) -> Pfunc r

andThen :: Decision token -> P token a -> P' token a
d `andThen` m = \cont s l -> unP m cont (PS s l mTok)
  where mTok = case d of Consume -> Nothing
                         PutBack tok -> Just tok

andReturn :: Decision token -> a -> P' token a
d `andReturn` a = d `andThen` return a

andThenJust :: Decision token -> P token a -> P' token (Maybe a)
d `andThenJust` p = d `andThen` fmap Just p

infix 0 `andThen`
infix 0 `andReturn`
infix 0 `andThenJust`

withToken :: HasLexer token => (token -> P' token a) -> P token a
withToken f =
  MkP $ \cont (PS s l mTok) ->
    case mTok of
      Nothing  -> lexToken (\tok -> f tok cont) s l
      Just tok -> f tok cont s l

newtype P token a =
  MkP { unP :: forall r. (a -> PState token -> ParseResult r) ->
                               PState token -> ParseResult r }

instance Functor (P token) where
  fmap = liftM

instance Applicative (P token) where
  pure a = MkP ($ a)
  MkP f <*> MkP v = MkP $ \cont -> f (\g -> v (cont . g))

instance Monad (P token) where
#if !MIN_VERSION_base(4,8,0)
  return = pure
#endif
  MkP m >>= k = MkP $ \cont -> m (\x -> unP (k x) cont)

failP :: (Int -> String) -> P token a
failP mkErr = MkP $ \_ (PS _ l _) -> Left (mkErr l)

lineP :: P token Int
lineP = MkP $ \cont pstate@(PS _ l _) -> cont l pstate

runP :: P token a -> PState token -> ParseResult a
runP (MkP p) = p (\a _ -> Right a)

manyP :: P token (Maybe a) -> P token [a]
manyP p = go []
  where
    go acc = do
      mX <- p
      case mX of
        Nothing -> return (reverse acc)
        Just x -> go (x : acc)

manySepByP :: HasLexer token => (token -> Bool) -> P token (Maybe a) -> P token [a]
manySepByP isSep p = go [] where
  go acc = do
    mX <- p
    case mX of
      Nothing -> return (reverse acc)
      Just x -> do
        let acc' = x : acc
        withToken $ \tok ->
          if isSep tok
          then Consume `andThen` go acc'
          else PutBack tok `andReturn` reverse acc'

someSepByP :: HasLexer token => (token -> Bool) -> P token a -> P token [a]
someSepByP isSep p = go [] where
  go acc = do
    x <- p
    let acc' = x : acc
    withToken $ \tok ->
      if isSep tok
      then Consume `andThen` go acc'
      else PutBack tok `andReturn` reverse acc'
