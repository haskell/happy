-- See <https://github.com/haskell/happy/issues/95> for more information
%name parse prod

%tokentype { Token }

%monad { P } { bindP } { returnP }
%error { error "parse error" }
%lexer { lexer } { EOF }

%token
  IDENT  { Identifier $$ }

%%

prod :: { () }
  : IDENT {%% \_ -> returnP () }

{

data Token = EOF | Identifier String

type P a = String -> (a, String)

bindP :: P a -> (a -> P b) -> P b
bindP p f s = let (x,s') = p s in f x s'

returnP :: a -> P a
returnP = (,)

lexer :: (Token -> P a) -> P a
lexer cont s = cont (case s of { "" -> EOF; _ -> Identifier s }) ""

main = pure ()

}
