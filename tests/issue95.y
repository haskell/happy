-- See <https://github.com/simonmar/happy/issues/95> for more information

#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

{

}

%name parse prod

%tokentype { Token }

%monad { P } { bindP } { returnP }
%error { QUALIFY(error) "parse error" }
%lexer { lexer } { EOF }

%token
  IDENT  { Identifier $$ }

%%

prod :: { () }
  : IDENT {%% \_ -> returnP () }

{

data Token = EOF | Identifier QUALIFY(String)

type P a = QUALIFY(String) -> (a, QUALIFY(String))

bindP :: P a -> (a -> P b) -> P b
bindP p f s = let (x,s') = p s in f x s'

returnP :: a -> P a
returnP = (,)

lexer :: (Token -> P a) -> P a
lexer cont s = cont (case s of { "" -> EOF; _ -> Identifier s }) ""

main = pure ()

}
