{-

With Happy 1.17 this file produces "Internal Happy error" when run:

$ happy ParGF.y && runghc ParGF.hs
ParGF.hs: Internal Happy error

The problem is that we always pass around the "current token".  When not
using %lexer and we've run out of tokens, the current token is notHappyAtAll,
which gets passed to happyError when there's an error.

-}

{
}

%name pGrammar

%tokentype { String }
%error { parseError }

%token 
 'a' { "a" }

%%

Grammar :: { () }
Grammar :  'a' 'a' { () } 

{

parseError :: [String] -> a
-- commenting out the below line gets rid of the "Internal Happy Error"
parseError ("":_) = error "bar"
parseError _ = error "foo"

main :: IO ()
main = print $ pGrammar ["a"]

}
