-- This parser parses the contents of the attribute grammar
-- into a list of rules.  A rule can either be an assignment
-- to an attribute of the LHS (synthesized attribute), and
-- assignment to an attribute of the RHS (an inherited attribute),
-- or a conditional statement.

module AttrGrammarParser.Oracle (agParser) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import ParseMonad
import AttrGrammar

type Parser = P AgToken

agParser :: Parser [AgRule]
agParser = manySepByP isSemi optRuleP

optRuleP :: Parser (Maybe AgRule)
optRuleP = withToken match where
  match (AgTok_SelfRef v) =
    Consume `andThenJust`
    pure (SelfAssign v) <* eqP <*> codeP
  match (AgTok_SubRef v) =
    Consume `andThenJust`
    pure (SubAssign v) <* eqP <*> codeP
  match (AgTok_RightmostRef v) =
    Consume `andThenJust`
    pure (RightmostAssign v) <* eqP <*> codeP
  match AgTok_Where =
    Consume `andThenJust`
    fmap Conditional codeP
  match tok = PutBack tok `andReturn` Nothing

eqP :: Parser AgToken
eqP = withToken match where
  match tok@AgTok_Eq = Consume `andReturn` tok
  match tok = PutBack tok `andThen` parseError "Expected '='"

rBraceP :: Parser AgToken
rBraceP = withToken match where
  match tok@AgTok_RBrace = Consume `andReturn` tok
  match tok = PutBack tok `andThen` parseError "Expected '}'"

codeP :: Parser [AgToken]
codeP = codeP' False

codeP' :: Bool -> Parser [AgToken]
codeP' consume_semi = withToken match where
  match tok@AgTok_LBrace =
    Consume `andThen` do
      c1 <- codeP' True
      tok' <- rBraceP
      c2 <- codeP' consume_semi
      return $ [tok] ++ c1 ++ [tok'] ++ c2
  match tok =
    let consume = Consume `andThen` do
          c <- codeP' consume_semi
          return (tok : c)
    in case tok of
      AgTok_Semicolon | consume_semi -> consume
      AgTok_Eq -> consume
      AgTok_SelfRef _ -> consume
      AgTok_SubRef _ -> consume
      AgTok_RightmostRef _ -> consume
      AgTok_Unknown _ -> consume
      _ -> PutBack tok `andReturn` []

isSemi :: AgToken -> Bool
isSemi AgTok_Semicolon = True
isSemi _ = False

parseError :: String -> Parser a
parseError s = failP $ \l -> show l ++ ": " ++ s ++ "\n"
