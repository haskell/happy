#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
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
  | Id QUALIFIEDPRELUDE.String
  | Num QUALIFIEDPRELUDE.Int
  | EOF
    deriving (QUALIFIEDPRELUDE.Eq, QUALIFIEDPRELUDE.Ord, QUALIFIEDPRELUDE.Show)

data AST =
    Sum AST AST
  | Prod AST AST
  | Neg AST
  | Var QUALIFIEDPRELUDE.String
  | Lit QUALIFIEDPRELUDE.Int
    deriving (QUALIFIEDPRELUDE.Eq, QUALIFIEDPRELUDE.Ord)

type Parser m = ExceptT () (Lexer m)

type Lexer m = StateT [Token] m

parseError :: MonadIO m => Token -> Parser m a
parseError tok =
  do
    liftIO (QUALIFIEDPRELUDE.putStrLn ("Parse error at " QUALIFIEDPRELUDE.++ QUALIFIEDPRELUDE.show tok))
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
       -> m (QUALIFIEDPRELUDE.Maybe AST)
parser input =
  let
    run :: (MonadIO m) =>
           Lexer m (QUALIFIEDPRELUDE.Maybe AST)
    run =
      do
        res <- runExceptT parse
        case res of
          QUALIFIEDPRELUDE.Left () -> QUALIFIEDPRELUDE.return QUALIFIEDPRELUDE.Nothing
          QUALIFIEDPRELUDE.Right ast -> QUALIFIEDPRELUDE.return (QUALIFIEDPRELUDE.Just ast)
  in do
    (out, _) <- runStateT run input
    QUALIFIEDPRELUDE.return out

main :: QUALIFIEDPRELUDE.IO ()
main =
  let
    input = [Id "x", Plus,
             Minus, Num 1, Times,
             LParen, Num 2, Plus, Id "y", RParen]
    expected = Sum (Var "x") (Prod (Neg (Lit 1)) (Sum (Lit 2) (Var "y")))
  in do
    res <- parser input
    case res of
      QUALIFIEDPRELUDE.Nothing -> QUALIFIEDPRELUDE.print "Test failed\n"
      QUALIFIEDPRELUDE.Just actual
        | expected QUALIFIEDPRELUDE.== actual -> QUALIFIEDPRELUDE.print "Test works\n"
        | QUALIFIEDPRELUDE.otherwise -> QUALIFIEDPRELUDE.print "Test failed\n"

-- vendored in parts of mtl

class QUALIFIEDPRELUDE.Monad m => MonadIO m where liftIO :: QUALIFIEDPRELUDE.IO a -> m a
instance MonadIO QUALIFIEDPRELUDE.IO where liftIO = QUALIFIEDPRELUDE.id

class QUALIFIEDPRELUDE.Monad m => MonadState s m | m -> s where
    put :: s -> m ()
    get :: m s

newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }

instance QUALIFIEDPRELUDE.Monad m => QUALIFIEDPRELUDE.Functor (StateT s m) where
    fmap = liftM

instance QUALIFIEDPRELUDE.Monad m => A.Applicative (StateT s m) where
    pure = QUALIFIEDPRELUDE.return
    (<*>) = ap

instance QUALIFIEDPRELUDE.Monad m => QUALIFIEDPRELUDE.Monad (StateT s m) where
    return x = StateT QUALIFIEDPRELUDE.$ \s -> QUALIFIEDPRELUDE.return (x, s)
    m >>= k = StateT QUALIFIEDPRELUDE.$ \s0 -> do
        (x, s1) <- runStateT m s0
        runStateT (k x) s1

instance QUALIFIEDPRELUDE.Monad m => MonadState s (StateT s m) where
    put s = StateT QUALIFIEDPRELUDE.$ \_ -> QUALIFIEDPRELUDE.return ((), s)
    get   = StateT QUALIFIEDPRELUDE.$ \s -> QUALIFIEDPRELUDE.return (s, s)

instance MonadIO m => MonadIO (StateT e m) where
    liftIO m = StateT QUALIFIEDPRELUDE.$ \s -> liftM (\x -> (x, s)) (liftIO m)

class QUALIFIEDPRELUDE.Monad m => MonadError e m | m -> e where
    throwError :: e -> m a

newtype ExceptT e m a = ExceptT { runExceptT :: m (QUALIFIEDPRELUDE.Either e a) }

instance QUALIFIEDPRELUDE.Monad m => QUALIFIEDPRELUDE.Functor (ExceptT e m) where
    fmap = liftM

instance QUALIFIEDPRELUDE.Monad m => A.Applicative (ExceptT e m) where
    pure = QUALIFIEDPRELUDE.return
    (<*>) = ap

instance QUALIFIEDPRELUDE.Monad m => QUALIFIEDPRELUDE.Monad (ExceptT e m) where
    return = ExceptT QUALIFIEDPRELUDE.. QUALIFIEDPRELUDE.return QUALIFIEDPRELUDE.. QUALIFIEDPRELUDE.Right
    m >>= k = ExceptT QUALIFIEDPRELUDE.$ do
        x <- runExceptT m
        case x of
            QUALIFIEDPRELUDE.Left e  -> QUALIFIEDPRELUDE.return (QUALIFIEDPRELUDE.Left e)
            QUALIFIEDPRELUDE.Right y -> runExceptT (k y)

instance MonadState s m => MonadState s (ExceptT e m) where
    put s = ExceptT (liftM QUALIFIEDPRELUDE.Right (put s))
    get   = ExceptT (liftM QUALIFIEDPRELUDE.Right get)

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO = ExceptT QUALIFIEDPRELUDE.. liftM QUALIFIEDPRELUDE.Right QUALIFIEDPRELUDE.. liftIO

instance QUALIFIEDPRELUDE.Monad m => MonadError e (ExceptT e m) where
    throwError = ExceptT QUALIFIEDPRELUDE.. QUALIFIEDPRELUDE.return QUALIFIEDPRELUDE.. QUALIFIEDPRELUDE.Left

}
