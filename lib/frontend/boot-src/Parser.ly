-----------------------------------------------------------------------------
$Id: Parser.ly,v 1.15 2005/01/26 01:10:42 ross Exp $

The parser.

(c) 1993-2000 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> {
> {-# OPTIONS_GHC -w #-}
> module Happy.Frontend.Parser (ourParser) where
> import Happy.Frontend.ParseMonad.Class
> import Happy.Frontend.ParseMonad
> import Happy.Frontend.AbsSyn
> import Happy.Frontend.Lexer
> }

> %name ourParser
> %tokentype { Token }
> %token
>       id              { TokenInfo $$ TokId }
>       spec_tokentype  { TokenKW      TokSpecId_TokenType }
>       spec_token      { TokenKW      TokSpecId_Token }
>       spec_name       { TokenKW      TokSpecId_Name }
>       spec_partial    { TokenKW      TokSpecId_Partial }
>       spec_lexer      { TokenKW      TokSpecId_Lexer }
>       spec_imported_identity  { TokenKW      TokSpecId_ImportedIdentity }
>       spec_monad      { TokenKW      TokSpecId_Monad }
>       spec_nonassoc   { TokenKW      TokSpecId_Nonassoc }
>       spec_left       { TokenKW      TokSpecId_Left }
>       spec_right      { TokenKW      TokSpecId_Right }
>       spec_prec       { TokenKW      TokSpecId_Prec }
>       spec_shift      { TokenKW      TokSpecId_Shift }
>       spec_expect     { TokenKW      TokSpecId_Expect }
>       spec_error      { TokenKW      TokSpecId_Error }
>       spec_errorexpected  { TokenKW      TokSpecId_ErrorExpected }
>       spec_errorhandlertype  { TokenKW      TokSpecId_ErrorHandlerType }
>       spec_attribute  { TokenKW      TokSpecId_Attribute }
>       spec_attributetype      { TokenKW      TokSpecId_Attributetype }
>       code            { TokenInfo $$ TokCodeQuote }
>       int             { TokenNum $$  TokNum }
>       ":"             { TokenKW      TokColon }
>       ";"             { TokenKW      TokSemiColon }
>       "::"            { TokenKW      TokDoubleColon }
>       "%%"            { TokenKW      TokDoublePercent }
>       "|"             { TokenKW      TokBar }
>       "("             { TokenKW      TokParenL }
>       ")"             { TokenKW      TokParenR }
>       ","             { TokenKW      TokComma }

> %monad { P }
> %lexer { lexTokenP } { TokenEOF }

> %%

> parser :: { BookendedAbsSyn }
>       : optCode core_parser optCode   { BookendedAbsSyn $1 $2 $3 }

> core_parser :: { AbsSyn String }
>       : tokInfos "%%" rules           { AbsSyn (reverse $1) (reverse $3) }

> rules :: { [Rule String] }
>       : rules rule    { $2 : $1 }
>       | rule          { [$1] }

> rule :: { Rule String }
>       : id params "::" code ":" prods         { Rule $1 $2 $6 (Just $4) }
>       | id params "::" code id ":" prods      { Rule $1 $2 $7 (Just $4) }
>       | id params ":" prods                   { Rule $1 $2 $4 Nothing }

> params :: { [String] }
>       : "(" comma_ids ")"             { reverse $2 }
>       | {- empty -}                   { [] }

> comma_ids :: { [String] }
>       : id                            { [$1] }
>       | comma_ids "," id              { $3 : $1 }

> prods :: { [Prod String] }
>       : prod "|" prods                { $1 : $3 }
>       | prod                          { [$1] }

> prod :: { Prod String }
>       : terms prec code ";"           {% lineP >>= \l -> return (Prod $1 $3 l $2) }
>       | terms prec code               {% lineP >>= \l -> return (Prod $1 $3 l $2) }

> term :: { Term }
>       : id                             { App $1 [] }
>       | id "(" comma_terms ")"         { App $1 (reverse $3) }

> terms :: { [Term] }
>       : terms_rev                      { reverse $1 }
>       |                                { [] }

> terms_rev :: { [Term] }
>       : term                           { [$1] }
>       | terms_rev term                 { $2 : $1 }

> comma_terms :: { [Term] }
>       : term                           { [$1] }
>       | comma_terms "," term           { $3 : $1 }

> prec :: { Prec }
>       : spec_prec id                  { PrecId $2 }
>       | spec_shift                    { PrecShift }
>       |                               { PrecNone }

> tokInfos :: { [Directive String] }
>       : tokInfos tokInfo              { $2 : $1 }
>       | tokInfo                       { [$1] }

> tokInfo :: { Directive String }
>       : spec_tokentype code           { TokenType $2 }
>       | spec_token tokenSpecs         { TokenSpec $2 }
>       | spec_name id optStart         { TokenName $2 $3 False }
>       | spec_partial id optStart      { TokenName $2 $3 True  }
>       | spec_imported_identity        { TokenImportedIdentity }
>       | spec_lexer code code          { TokenLexer $2 $3 }
>       | spec_monad code               { TokenMonad "()" $2 "Happy_Prelude.>>=" "Happy_Prelude.return" }
>       | spec_monad code code          { TokenMonad $2 $3 "Happy_Prelude.>>=" "Happy_Prelude.return" }
>       | spec_monad code code code     { TokenMonad "()" $2 $3 $4 }
>       | spec_monad code code code code        { TokenMonad $2 $3 $4 $5 }
>       | spec_nonassoc ids             { TokenNonassoc $2 }
>       | spec_right ids                { TokenRight $2 }
>       | spec_left ids                 { TokenLeft $2 }
>       | spec_expect int               { TokenExpect $2 }
>       | spec_error code optCode       { TokenError $2 $3 }
>       | spec_errorexpected            { TokenErrorExpected }
>       | spec_errorhandlertype id      { TokenErrorHandlerType $2 }
>       | spec_attributetype code       { TokenAttributetype $2 }
>       | spec_attribute id code        { TokenAttribute $2 $3 }

> optStart :: { Maybe String }
>       : id                            { Just $1 }
>       | {- nothing -}                 { Nothing }

> tokenSpecs :: { [(String, TokenSpec)] }
>       : tokenSpec tokenSpecs          { $1:$2 }
>       | tokenSpec                     { [$1] }

> tokenSpec :: { (String, TokenSpec) }
>       : id code                       { ($1, parseTokenSpec $2) }

> ids   :: { [String] }
>       : id ids                        { $1 : $2 }
>       | {- nothing -}                 { [] }

> optCode :: { Maybe String }
>       : code                          { Just $1 }
>       | {- nothing -}                 { Nothing }

> {
> happyError :: P a
> happyError = failP (\l -> show l ++ ": Parse error\n")
> }
