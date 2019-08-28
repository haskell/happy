{
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
import Control.Monad (liftM, ap)
import Control.Applicative as A
}

%name parse exp
%tokentype { Token }
%error { parseError }
%monad { (MonadIO m) } { Parser m }
%lexer { lexer } { EOF }
%token ID { Id _ }
       NUM { Num _ }
       PLUS { Plus }
       MINUS { Minus }
       TIMES { Times }
       LPAREN { LParen }
       RPAREN { RParen }

%%

exp :: { AST }
    : exp PLUS prod
      { Sum $1 $3 }
    | prod
      { $1 }

prod :: { AST }
     : prod TIMES neg
       { Prod $1 $3 }
     | neg
       { $1 }

neg :: { AST }
    : MINUS neg
      { Neg $2 }
    | atom
      { $1 }

atom :: { AST }
     : ID
       { let Id str = $1 in Var str }
     | NUM
       { let Num n = $1 in Lit n }
     | LPAREN exp RPAREN
       { $2 }

{

data Token =
    Plus
  | Minus
  | Times
  | LParen
  | RParen
  | Id String
  | Num Int
  | EOF
    deriving (Eq, Ord, Show)

data AST =
    Sum AST AST
  | Prod AST AST
  | Neg AST
  | Var String
  | Lit Int
    deriving (Eq, Ord)

type Parser m = ExceptT () (Lexer m)

type Lexer m = StateT [Token] m

parseError :: MonadIO m => Token -> Parser m a
parseError tok =
  do
    liftIO (putStrLn ("Parse error at " ++ show tok))
    throwError ()

lexer :: MonadIO m => (Token -> Parser m a) -> Parser m a
lexer cont =
  do
    toks <- get
    case toks of
      [] -> cont EOF
      first : rest ->
        do
          put rest
          cont first

parse :: (MonadIO m) => Parser m AST

parser :: (MonadIO m) =>
          [Token]
       -> m (Maybe AST)
parser input =
  let
    run :: (MonadIO m) =>
           Lexer m (Maybe AST)
    run =
      do
        res <- runExceptT parse
        case res of
          Left () -> return Nothing
          Right ast -> return (Just ast)
  in do
    (out, _) <- runStateT run input
    return out

main :: IO ()
main =
  let
    input = [Id "x", Plus,
             Minus, Num 1, Times,
             LParen, Num 2, Plus, Id "y", RParen]
    expected = Sum (Var "x") (Prod (Neg (Lit 1)) (Sum (Lit 2) (Var "y")))
  in do
    res <- parser input
    case res of
      Nothing -> print "Test failed\n"
      Just actual
        | expected == actual -> print "Test works\n"
        | otherwise -> print "Test failed\n"

-- vendored in parts of mtl

class Monad m => MonadIO m where liftIO :: IO a -> m a
instance MonadIO IO where liftIO = id

class Monad m => MonadState s m | m -> s where
    put :: s -> m ()
    get :: m s

newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance Monad m => A.Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    return x = StateT $ \s -> return (x, s)
    m >>= k = StateT $ \s0 -> do
        (x, s1) <- runStateT m s0
        runStateT (k x) s1

instance Monad m => MonadState s (StateT s m) where
    put s = StateT $ \_ -> return ((), s)
    get   = StateT $ \s -> return (s, s)

instance MonadIO m => MonadIO (StateT e m) where
    liftIO m = StateT $ \s -> liftM (\x -> (x, s)) (liftIO m)

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Monad m => Functor (ExceptT e m) where
    fmap = liftM

instance Monad m => A.Applicative (ExceptT e m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
    return = ExceptT . return . Right
    m >>= k = ExceptT $ do
        x <- runExceptT m
        case x of
            Left e  -> return (Left e)
            Right y -> runExceptT (k y)

instance MonadState s m => MonadState s (ExceptT e m) where
    put s = ExceptT (liftM Right (put s))
    get   = ExceptT (liftM Right get)

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO = ExceptT . liftM Right . liftIO

instance Monad m => MonadError e (ExceptT e m) where
    throwError = ExceptT . return . Left

}
