--
-- Lexical syntax for Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
module Main (main) where
import Data.Char ( ord )
}

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit	   = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

%reservedid = 
	as|case|class|data|default|deriving|do|else|hiding|if|
	import|in|infix|infixl|infixr|instance|let|module|newtype|
	of|qualified|then|type|where

%reservedop =
	".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

%varid  = $small $idchar*
%conid  = $large $idchar*
%varsym = $symbol $symchar*
%consym = \: $symchar*

%decimal     = $digit+
%octal       = $octit+
%hexadecimal = $hexit+
%exponent    = [eE] [\-\+] %decimal

$cntrl   = [$large \@\[\\\]\^\_]
%ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
%escape  = \\ ($charesc | %ascii | %decimal | o %octal | x %hexadecimal)
%gap     = \\ $whitechar+ \\
%string  = $graphic # [\"\\] | " " | %escape | %gap

haskell :-

<0> $white+			{ skip }
<0> "--"\-*[^$symbol].*		{ skip }

"{-"				{ nested_comment }

<0> $special			{ mkL LSpecial }

<0> %reservedid			{ mkL LReservedId }
<0> %conid \. %varid		{ mkL LQVarId }
<0> %conid \. %conid		{ mkL LQConId }
<0> %varid			{ mkL LVarId }
<0> %conid			{ mkL LConId }

<0> %reservedop			{ mkL LReservedOp }
<0> %conid \. %varsym		{ mkL LVarSym }
<0> %conid \. %consym		{ mkL LConSym }
<0> %varsym			{ mkL LVarSym }
<0> %consym			{ mkL LConSym }

<0> %decimal 
  | 0[oO] %octal
  | 0[xX] %hexadecimal		{ mkL LInteger }

<0> %decimal \. %decimal %exponent?
  | %decimal %exponent		{ mkL LFloat }

<0> \' ($graphic # [\'\\] | " " | %escape) \'
				{ mkL LChar }

<0> \" %string* \"		{ mkL LString }

{
data Lexeme = L AlexPosn LexemeClass String

data LexemeClass
  = LInteger
  | LFloat
  | LChar
  | LString
  | LSpecial
  | LReservedId
  | LReservedOp
  | LVarId
  | LQVarId
  | LConId
  | LQConId
  | LVarSym
  | LQVarSym
  | LConSym
  | LQConSym
  | LEOF
  deriving Eq
  
mkL :: LexemeClass -> (AlexPosn,String) -> Int -> Alex Lexeme
mkL c (p,str) len = return (L p c (take len str))

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = go 1
  where go 0 = alexScan
	go n = do
	  m <- alexGetChar
	  case m of
	    Nothing  -> err
	    Just '-' -> do
		m <- alexGetChar
		case m of
		   Nothing  -> err
		   Just '\125' -> go (n-1)
		   Just c   -> go n
	    Just '\123' -> do
		m <- alexGetChar
		case m of
		   Nothing  -> err
		   Just '-' -> go (n+1)
		   Just c   -> go n
	    Just c -> go n

        err = lexError "error in nested comment"  

lexError s = do
  (p,input) <- alexGetInput
  error (showPosn p ++ ": " ++ s ++ 
		(if (not (null input))
		  then " before " ++ show (head input)
		  else " at end of file"))

scanner str = runAlex str $ do
  let loop i = do tok@(L _ cl _) <- alexScan; 
		  if cl == LEOF
			then return i
			else do let i' = i+1 in i' `seq` loop i'
  loop 0

alexEOF (p,"")   = return (L p LEOF "")
alexEOF (p,rest) = lexError "lexical error"

showPosn (AlexPn _ line col) = show line ++ ':': show col

main = do
  s <- getContents
  print (scanner s)
}
