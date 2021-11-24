{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Happy.Frontend.Parser.Oracle (ourParser, AbsSyn) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Monad (when)
import Data.Maybe (isJust)
import Happy.Frontend.ParseMonad.Class
import Happy.Frontend.ParseMonad.Oracle
import Happy.Frontend.AbsSyn
import Happy.Frontend.Lexer

type Parser = P Token

ourParser :: Parser AbsSyn
ourParser = do
  headerCode <- optCodeP
  tokInfos <- manyP optTokInfoP
  expectKW "Expected %%" TokDoublePercent
  rules <- rulesP
  footerCode <- optCodeP
  eofP
  return (AbsSyn headerCode tokInfos rules footerCode)

optCodeP :: Parser (Maybe String)
optCodeP = withToken match where
  match :: Token -> P' Token (Maybe String)
  match (TokenInfo str TokCodeQuote) = Consume `andReturn` Just str
  match tok = PutBack tok `andReturn` Nothing

codeP :: Parser String
codeP = do
  mCode <- optCodeP
  case mCode of
    Nothing -> parseError "Expected a code block"
    Just code -> return code

optTokInfoP :: Parser (Maybe (Directive String))
optTokInfoP = withToken match where
  match :: Token -> P' Token (Maybe (Directive String))
  match (TokenKW TokSpecId_TokenType) =
    Consume `andThenJust`
    pure TokenType <*> codeP
  match (TokenKW TokSpecId_Token) =
    Consume `andThenJust`
    pure TokenSpec <*> manyP optTokenSpecP
  match (TokenKW TokSpecId_Name) =
    Consume `andThenJust`
    pure TokenName <*> idtP <*> optIdtP <*> pure False
  match (TokenKW TokSpecId_Partial) =
    Consume `andThenJust`
    pure TokenName <*> idtP <*> optIdtP <*> pure True
  match (TokenKW TokSpecId_ImportedIdentity) =
    Consume `andThenJust`
    pure TokenImportedIdentity
  match (TokenKW TokSpecId_Lexer) =
    Consume `andThenJust`
    pure TokenLexer <*> codeP <*> codeP
  match (TokenKW TokSpecId_Monad) =
    Consume `andThenJust` do
      codes <- manyP optCodeP
      case codes of
        [c1]             -> return $ TokenMonad "()" c1 "Prelude.>>=" "Prelude.return"
        [c1, c2]         -> return $ TokenMonad c1 c2 "Prelude.>>=" "Prelude.return"
        [c1, c2, c3]     -> return $ TokenMonad "()" c1 c2 c3
        [c1, c2, c3, c4] -> return $ TokenMonad c1 c2 c3 c4
        [] -> parseError "Expected a code block"
        _  -> parseError "Too many code blocks"
  match (TokenKW TokSpecId_Nonassoc) =
    Consume `andThenJust`
    pure TokenNonassoc <*> manyP optIdtP
  match (TokenKW TokSpecId_Right) =
    Consume `andThenJust`
    pure TokenRight <*> manyP optIdtP
  match (TokenKW TokSpecId_Left) =
    Consume `andThenJust`
    pure TokenLeft <*> manyP optIdtP
  match (TokenKW TokSpecId_Expect) =
    Consume `andThenJust`
    pure TokenExpect <*> numP
  match (TokenKW TokSpecId_Error) =
    Consume `andThenJust`
    pure TokenError <*> codeP
  match (TokenKW TokSpecId_ErrorHandlerType) =
    Consume `andThenJust`
    pure TokenErrorHandlerType <*> idtP
  match (TokenKW TokSpecId_Attributetype) =
    Consume `andThenJust`
    pure TokenAttributetype <*> codeP
  match (TokenKW TokSpecId_Attribute) =
    Consume `andThenJust`
    pure TokenAttribute <*> idtP <*> codeP
  match tok = PutBack tok `andReturn` Nothing

optIdtP :: Parser (Maybe String)
optIdtP = withToken match where
  match :: Token -> P' Token (Maybe String)
  match (TokenInfo idt TokId) = Consume `andReturn` Just idt
  match tok = PutBack tok `andReturn` Nothing

idtP :: Parser String
idtP = do
  mIdt <- optIdtP
  case mIdt of
    Nothing -> parseError "Expected an identifier"
    Just idt -> return idt

numP :: Parser Int
numP = withToken match where
  match :: Token -> P' Token Int
  match (TokenNum n TokNum) = Consume `andReturn` n
  match tok = PutBack tok `andThen` parseError "Expected a number"

optTokenSpecP :: Parser (Maybe (String, String))
optTokenSpecP = withToken match where
  match :: Token -> P' Token (Maybe (String, String))
  match (TokenInfo idt TokId) =
    Consume `andThenJust` do
      code <- codeP
      return (idt, code)
  match tok = PutBack tok `andReturn` Nothing

rulesP :: Parser [Rule]
rulesP = do
  rules <- manyP optRuleP
  when (null rules) (parseError "At least one rule required")
  return rules

optRuleP :: Parser (Maybe Rule)
optRuleP = do
  mIdt <- optIdtP
  case mIdt of
    Nothing -> return Nothing
    Just idt -> do
      params <- paramsP idtP
      mSig <- optSigP
      mIdt' <- if isJust mSig then optIdtP else return Nothing
      case mIdt' of
        Just idt' | idt' /= idt ->
          parseError "Name mismatch in signature and definition"
        _ -> return ()
      expectKW "Expected ':'" TokColon
      prods <- someSepByP (isKW TokBar) prodP
      let rule = Rule idt params prods mSig
      return (Just rule)

optSigP :: Parser (Maybe String)
optSigP = withToken match where
  match :: Token -> P' Token (Maybe String)
  match (TokenKW TokDoubleColon) = Consume `andThenJust` codeP
  match tok = PutBack tok `andReturn` Nothing

paramsP :: forall a. Parser a -> Parser [a]
paramsP p = withToken match where
  match :: Token -> P' Token [a]
  match (TokenKW TokParenL) =
    Consume `andThen` do
      params <- someSepByP (isKW TokComma) p
      expectKW "Expected ')'" TokParenR
      return params
  match tok = PutBack tok `andReturn` []

optSemiP :: Parser ()
optSemiP = withToken match where
  match :: Token -> P' Token ()
  match (TokenKW TokSemiColon) = Consume `andReturn` ()
  match tok = PutBack tok `andReturn` ()

prodP :: Parser Prod
prodP = do
  terms <- manyP optTermP
  prec <- precP
  code <- codeP
  optSemiP
  l <- lineP
  return (Prod terms code l prec)

termP :: Parser Term
termP = do
  mTerm <- optTermP
  case mTerm of
    Nothing -> parseError "Expected a term"
    Just term -> return term

optTermP :: Parser (Maybe Term)
optTermP = withToken match where
  match :: Token -> P' Token (Maybe Term)
  match (TokenInfo idt TokId) =
    Consume `andThenJust` do
      termParams <- paramsP termP
      return (App idt termParams)
  match tok = PutBack tok `andReturn` Nothing

precP :: Parser Prec
precP = withToken match where
  match :: Token -> P' Token Prec
  match (TokenKW TokSpecId_Shift) = Consume `andReturn` PrecShift
  match (TokenKW TokSpecId_Prec) = Consume `andThen` fmap PrecId idtP
  match tok = PutBack tok `andReturn` PrecNone

eofP :: Parser ()
eofP = withToken match where
  match :: Token -> P' Token ()
  match (TokenEOF) = Consume `andReturn` ()
  match tok = PutBack tok `andThen` parseError "Parse error"

parseError :: String -> Parser a
parseError s = failP $ \l -> show l ++ ": " ++ s ++ "\n"

isKW :: TokenId -> Token -> Bool
isKW tokId (TokenKW tokId') = tokId == tokId'
isKW _ _ = False

expectKW :: String -> TokenId -> Parser ()
expectKW err_msg kw = withToken match where
  match :: Token -> P' Token ()
  match (TokenKW tokId) | tokId == kw = Consume `andReturn` ()
  match tok = PutBack tok `andThen` parseError err_msg
