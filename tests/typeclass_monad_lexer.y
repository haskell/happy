#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

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

}
