{
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans

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

}