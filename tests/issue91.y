-- See <https://github.com/simonmar/happy/issues/91> for more information

#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{

}

%name parse prod

%tokentype { Tok }

%monad { P } { bindP } { returnP }
%error { QUALIFIEDPRELUDE.error "parse error" }
%lexer { lexer } { EOF }

%token
  IDENT  { Identifier $$ }

%%

prod :: { () }
  : IDENT { () }

{

data Tok = EOF | Identifier QUALIFIEDPRELUDE.String

type P a = QUALIFIEDPRELUDE.String -> (a, QUALIFIEDPRELUDE.String)

bindP :: P a -> (a -> P b) -> P b
bindP p f s = let (x,s') = p s in f x s'

returnP :: a -> P a
returnP = (,)

lexer :: (Tok -> P a) -> P a
lexer cont s = cont (case s of { "" -> EOF; _ -> Identifier s }) ""

main = QUALIFIEDPRELUDE.pure ()

}
