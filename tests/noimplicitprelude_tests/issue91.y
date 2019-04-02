-- See <https://github.com/simonmar/happy/issues/91> for more information

{
{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
}

%name parse prod

%tokentype { Tok }

%monad { P } { bindP } { returnP }
%error { Prelude.error "parse error" }
%lexer { lexer } { EOF }

%token
  IDENT  { Identifier $$ }

%%

prod :: { () }
  : IDENT { () }

{

data Tok = EOF | Identifier Prelude.String

type P a = Prelude.String -> (a, Prelude.String)

bindP :: P a -> (a -> P b) -> P b
bindP p f s = let (x,s') = p s in f x s'

returnP :: a -> P a
returnP = (,)

lexer :: (Tok -> P a) -> P a
lexer cont s = cont (case s of { "" -> EOF; _ -> Identifier s }) ""

main = Prelude.pure ()

}
