{
module Parser (parse, P(..)) where
import AbsSyn
import Scan
import CharSet

import Data.FiniteMap
import Data.Char
--import Debug.Trace
}

%tokentype { Token }

%name parse

%monad { P } { (>>=) } { return }

%token
	'.'		{ T _ (SpecialT '.') }
	';'		{ T _ (SpecialT ';') }
	'<'		{ T _ (SpecialT '<') }
	'>'		{ T _ (SpecialT '>') }
	','		{ T _ (SpecialT ',') }
	'/'		{ T _ (SpecialT '/') }
	'\\'		{ T _ (SpecialT '\\') }
	'|'		{ T _ (SpecialT '|') }
	'*'		{ T _ (SpecialT '*') }
	'+'		{ T _ (SpecialT '+') }
	'?'		{ T _ (SpecialT '?') }
	'{'		{ T _ (SpecialT '{') }
	'}'		{ T _ (SpecialT '}') }
	'('		{ T _ (SpecialT '(') }
	')'		{ T _ (SpecialT ')') }
	'#'		{ T _ (SpecialT '#') }
	'~'		{ T _ (SpecialT '~') }
	'-'		{ T _ (SpecialT '-') }
	'['		{ T _ (SpecialT '[') }
	']'		{ T _ (SpecialT ']') }
	'^'		{ T _ (SpecialT '^') }
	ZERO		{ T _ ZeroT }
	STRING		{ T _ (StringT $$) }
	BIND		{ T _ (BindT $$) }
	ID		{ T _ (IdT $$) }
	CODE		{ T _ (CodeT $$) }
	CHAR		{ T _ (CharT $$) }
	SMAC		{ T _ (SMacT _) }
	RMAC		{ T _ (RMacT $$) }
	SMAC_DEF	{ T _ (SMacDefT $$) }
	RMAC_DEF	{ T _ (RMacDefT $$) }
%%

alex	:: { [Def] }
	: def alex			{ $1 : $2 }
	| macdef alex			{ $2 }
	| def				{ [$1] }

def	:: { Def }
	: scanner			{ DefScanner $1 }
	| CODE				{ DefCode    $1 }

-- hack: the lexer looks for the '=' in a macro definition, because there
-- doesn't seem to be a way to formulate the grammar here to avoid a
-- conflict (it needs LR(2) rather than LR(1) to find the '=' and distinguish
-- an SMAC/RMAC at the beginning of a definition from an SMAC/RMAC that is
-- part of a regexp in the previous definition).
macdef	:: { () }
	: SMAC_DEF set			{% newSMac $1 $2 }
	| RMAC_DEF rexp			{% newRMac $1 $2 }

scanner	:: { Scanner }
	: BIND tokendefs	 	{ Scanner $1 $2 }

tokendefs :: { [RECtx] }
	: tokendef tokendefs		{ $1 : $2 }
	| {- empty -}			{ [] }

tokendef :: { RECtx }
	: startcodes context rhs	{ let (l,e,r) = $2 in 
					  RECtx $1 l e r $3 }
	| context rhs			{ let (l,e,r) = $1 in 
					  RECtx [] l e r $2 }

startcodes :: { [(String,StartCode)] }
	: '<' startcodes0 '>' 		{ $2 }

startcodes0 :: { [(String,StartCode)] }
	: startcode ',' startcodes0 	{ ($1,0) : $3 }
	| startcode 			{ [($1,0)] }

startcode :: { String }
	: ZERO 				{ "0" }
	| ID	 			{ $1 }

rhs	:: { Code }
	: CODE 				{ $1 }
	| ';'	 			{ "" }

context :: { Maybe CharSet, RExp, Maybe RExp }
	: rexp 				{ (Nothing,$1,Nothing) }
	| rexp '/' rexp 		{ (Nothing,$1,Just $3) }
	| set '\\' rexp			{ (Just $1,$3,Nothing) }
	| set '\\' rexp '/' rexp 	{ (Just $1,$3,Just $5) }

rexp	:: { RExp }
	: alt '|' rexp 			{ $1 :| $3 }
	| alt		 		{ $1 }

alt	:: { RExp }
	: alt term  			{ $1 :%% $2 }
	| term 				{ $1 }

term	:: { RExp }
	: rexp0 rep 			{ $2 $1 }
	| rexp0 			{ $1 }

rep	:: { RExp -> RExp }
	: '*' 				{ Star }
	| '+' 				{ Plus }
	| '?' 				{ Ques }
					-- TODO: these don't check for digits
					-- properly.
	| '{' CHAR '}'			{ repeat_rng (digit $2) Nothing }
	| '{' CHAR ',' '}'		{ repeat_rng (digit $2) (Just Nothing) }
	| '{' CHAR ',' CHAR '}' 	{ repeat_rng (digit $2) (Just (Just (digit $4))) }

rexp0	:: { RExp }
	: '(' ')'  			{ Eps }
	| STRING			{ foldr (:%%) Eps 
					    (map (Ch . charSetSingleton) $1) }
	| RMAC 				{% lookupRMac $1 }
	| set 				{ Ch $1 }
	| '(' rexp ')' 			{ $2 }

set	:: { CharSet }
 	: set '#' set0 			{ $1 `charSetMinus` $3 }
	| set0 				{ $1 }

set0	:: { CharSet }
	: '~' set0 			{ charSetComplement $2 }
	| CHAR 				{ charSetSingleton $1 }
	| CHAR '-' CHAR			{ charSetRange $1 $3 }
	| smac 				{% lookupSMac $1 }
	| '[' sets ']' 			{ foldr charSetUnion emptyCharSet $2 }
	| '[' '^' sets ']'		{ charSetComplement $
					   foldr charSetUnion emptyCharSet $3 }

sets	:: { [CharSet] }
	: set sets			{ $1 : $2 }
	| set				{ [$1] }

smac	:: { (Posn,String) }
 	: '.'				{ (tokPosn $1, ".") }
	| SMAC				{ case $1 of T p (SMacT s) -> (p, s) }

{
-- ---------------------------------------------------------------------------
-- The parsing monad

type Env = (FiniteMap String CharSet, FiniteMap String RExp)

type ParseError = (Maybe Posn,String)

newtype P a = P { unP :: Env -> Either ParseError (Env,a) }

instance Monad P where
 (P m) >>= k = P $ \env -> case m env of
			Left err -> Left err
			Right (env',ok) -> unP (k ok) env'
 return a = P $ \env -> Right (env,a)

-- Macros are expanded during parsing, to simplify the abstract
-- syntax.  The parsing monad passes around two environments mapping
-- macro names to sets and regexps respectively.

lookupSMac :: (Posn,String) -> P CharSet
lookupSMac (posn,smac)
 = P $ \env@(senv,_) -> 
       case lookupFM senv smac of
	Just ok -> Right (env,ok)
	Nothing -> Left (Just posn, "unknown set macro: $" ++ smac)

lookupRMac :: String -> P RExp
lookupRMac rmac 
 = P $ \env@(_,renv) -> 
       case lookupFM renv rmac of
	Just ok -> Right (env,ok)
	Nothing -> Left (Nothing, "unknown regex macro: %" ++ rmac)

newSMac :: String -> CharSet -> P ()
newSMac smac set 
  = P $ \(senv,renv) -> Right ((addToFM senv smac set, renv), ())

newRMac :: String -> RExp -> P ()
newRMac rmac rexp 
  = P $ \(senv,renv) -> Right ((senv, addToFM renv rmac rexp), ())

happyError :: [Token] -> P a
happyError (T p tkn : _) 
 = P $ \env -> Left (Just p, "parse error: " ++ show tkn)
happyError [] =  P $ \env -> Left (Nothing, "parse error")

-- -----------------------------------------------------------------------------
-- Utils

digit c = ord c - ord '0'

repeat_rng :: Int -> Maybe (Maybe Int) -> (RExp->RExp)
repeat_rng n (Nothing) re = foldr (:%%) Eps (replicate n re)
repeat_rng n (Just Nothing) re = foldr (:%%) (Star re) (replicate n re)
repeat_rng n (Just (Just m)) re = intl :%% rst
	where
	intl = repeat_rng n Nothing re
	rst = foldr (\re re'->Ques(re :%% re')) Eps (replicate (m-n) re)
}
