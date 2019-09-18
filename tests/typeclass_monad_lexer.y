#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

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
  | Id QUALIFY(String)
  | Num QUALIFY(Int)
  | EOF
    deriving (QUALIFY(Eq), QUALIFY(Ord), QUALIFY(Show))

data AST =
    Sum AST AST
  | Prod AST AST
  | Neg AST
  | Var QUALIFY(String)
  | Lit QUALIFY(Int)
    deriving (QUALIFY(Eq), QUALIFY(Ord))

type Parser m = ExceptT () (Lexer m)

type Lexer m = StateT [Token] m

parseError :: MonadIO m => Token -> Parser m a
parseError tok =
  do
    liftIO (QUALIFY(putStrLn) ("Parse error at " QUALIFY(++) QUALIFY(show) tok))
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
       -> m (QUALIFY(Maybe) AST)
parser input =
  let
    run :: (MonadIO m) =>
           Lexer m (QUALIFY(Maybe) AST)
    run =
      do
        res <- runExceptT parse
        case res of
          QUALIFY(Left) () -> QUALIFY(return) QUALIFY(Nothing)
          QUALIFY(Right) ast -> QUALIFY(return) (QUALIFY(Just) ast)
  in do
    (out, _) <- runStateT run input
    QUALIFY(return) out

main :: QUALIFY(IO) ()
main =
  let
    input = [Id "x", Plus,
             Minus, Num 1, Times,
             LParen, Num 2, Plus, Id "y", RParen]
    expected = Sum (Var "x") (Prod (Neg (Lit 1)) (Sum (Lit 2) (Var "y")))
  in do
    res <- parser input
    case res of
      QUALIFY(Nothing) -> QUALIFY(print) "Test failed\n"
      QUALIFY(Just) actual
        | expected QUALIFY(==) actual -> QUALIFY(print) "Test works\n"
        | QUALIFY(otherwise) -> QUALIFY(print) "Test failed\n"

-- vendored in parts of mtl

class QUALIFY(Monad) m => MonadIO m where liftIO :: QUALIFY(IO) a -> m a
instance MonadIO QUALIFY(IO) where liftIO = QUALIFY(id)

class QUALIFY(Monad) m => MonadState s m | m -> s where
    put :: s -> m ()
    get :: m s

newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }

instance QUALIFY(Monad) m => QUALIFY(Functor) (StateT s m) where
    fmap = liftM

instance QUALIFY(Monad) m => A.Applicative (StateT s m) where
    pure = QUALIFY(return)
    (<*>) = ap

instance QUALIFY(Monad) m => QUALIFY(Monad) (StateT s m) where
    return x = StateT QUALIFY($) \s -> QUALIFY(return) (x, s)
    m >>= k = StateT QUALIFY($) \s0 -> do
        (x, s1) <- runStateT m s0
        runStateT (k x) s1

instance QUALIFY(Monad) m => MonadState s (StateT s m) where
    put s = StateT QUALIFY($) \_ -> QUALIFY(return) ((), s)
    get   = StateT QUALIFY($) \s -> QUALIFY(return) (s, s)

instance MonadIO m => MonadIO (StateT e m) where
    liftIO m = StateT QUALIFY($) \s -> liftM (\x -> (x, s)) (liftIO m)

class QUALIFY(Monad) m => MonadError e m | m -> e where
    throwError :: e -> m a

newtype ExceptT e m a = ExceptT { runExceptT :: m (QUALIFY(Either) e a) }

instance QUALIFY(Monad) m => QUALIFY(Functor) (ExceptT e m) where
    fmap = liftM

instance QUALIFY(Monad) m => A.Applicative (ExceptT e m) where
    pure = QUALIFY(return)
    (<*>) = ap

instance QUALIFY(Monad) m => QUALIFY(Monad) (ExceptT e m) where
    return = ExceptT QUALIFY(.) QUALIFY(return) QUALIFY(.) QUALIFY(Right)
    m >>= k = ExceptT QUALIFY($) do
        x <- runExceptT m
        case x of
            QUALIFY(Left) e  -> QUALIFY(return) (QUALIFY(Left) e)
            QUALIFY(Right) y -> runExceptT (k y)

instance MonadState s m => MonadState s (ExceptT e m) where
    put s = ExceptT (liftM QUALIFY(Right) (put s))
    get   = ExceptT (liftM QUALIFY(Right) get)

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO = ExceptT QUALIFY(.) liftM QUALIFY(Right) QUALIFY(.) liftIO

instance QUALIFY(Monad) m => MonadError e (ExceptT e m) where
    throwError = ExceptT QUALIFY(.) QUALIFY(return) QUALIFY(.) QUALIFY(Left)

}
