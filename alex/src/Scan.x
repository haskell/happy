-------------------------------------------------------------------------------
--		    ALEX SCANNER AND LITERATE PREPROCESSOR
-- 
-- This Script defines the grammar used to generate the Alex scanner and a
-- preprocessing scanner for dealing with literate scripts.  The actions for
-- the Alex scanner are given separately in the Alex module.
--  
-- See the Alex manual for a discussion of the scanners defined here.
--  
-- Chris Dornan, Aug-95, 4-Jun-96, 10-Jul-96, 29-Sep-97
-------------------------------------------------------------------------------

{
module Scan(lexer, AlexPosn(..), Token(..), Tkn(..), tokPosn) where

import Data.Char
import Debug.Trace
}

$digit    = 0-9
$hexdig   = [0-9 A-F a-f]
$octal    = 0-7
$lower    = a-z
$upper    = A-Z
$alpha    = [$upper $lower]
$alphanum = [$alpha $digit]
$idchar   = [$alphanum \_ \']

%id     = $alpha $idchar*
%smac   = \$ %id | \$ \{ %id \}
%rmac   = \% %id | \% \{ %id \}

%comment = \-\-.*
%ws      = $white+ | %comment

alex :-

%ws				{ skip }	-- white space; ignore

<0> \. | \; | \, | \$
  | \| | \* | \+ | \? | \#
  | \~ | \- | \}
  | \( | \) | \[ | \] | \^	{ special }
<0> \{				{ brace }

<0> \" [^\"]* \"		{ string }
<0> %id %ws? \:\-		{ bind }
<0> \\ $digit+			{ decch }
<0> \\ x $hexdig+		{ hexch }
<0> \\ o $octal+		{ octch }
<0> \\ $printable		{ escape }
<0> $alphanum			{ char }
<0> %smac			{ smac }
<0> %rmac			{ rmac }
<0> %smac %ws? \=		{ smacdef }
<0> %rmac %ws? \=		{ rmacdef }

<0> \{ [^$digit \}]		{ code }

-- identifiers are allowed to be unquoted in startcode lists
<0> 		\< 		{ special `andBegin` startcodes }
<startcodes>	0		{ zero }
<startcodes>	%id		{ startcode }
<startcodes>	\,		{ special }
<startcodes> 	\> 		{ special `andBegin` 0 }

{
-- -----------------------------------------------------------------------------
-- Token type

data Token = T AlexPosn Tkn
  deriving Show

tokPosn (T p _) = p

data Tkn
 = SpecialT Char
 | CodeT String
 | ZeroT
 | IdT String
 | StringT String
 | BindT String
 | CharT Char
 | SMacT String
 | RMacT String  
 | SMacDefT String
 | RMacDefT String  
 | NumT Int	
 | EOFT
 deriving Show

-- -----------------------------------------------------------------------------
-- Token functions

special   (p,str) ln = return $ T p (SpecialT  (head str))
brace     (p,str) ln = return $ T p (SpecialT  '{')
zero      (p,str) ln = return $ T p ZeroT
string    (p,str) ln = return $ T p (StringT (extract ln str))
bind      (p,str) ln = return $ T p (BindT (takeWhile isIdChar str))
escape    (p,str) ln = return $ T p (CharT (esc str))
decch     (p,str) ln = return $ T p (CharT (do_ech 10 ln (take (ln-1) (tail str))))
hexch     (p,str) ln = return $ T p (CharT (do_ech 16 ln (take (ln-2) (drop 2 str))))
octch     (p,str) ln = return $ T p (CharT (do_ech 8  ln (take (ln-2) (drop 2 str))))
char      (p,str) ln = return $ T p (CharT (head str))
smac      (p,str) ln = return $ T p (SMacT (mac ln str))
rmac      (p,str) ln = return $ T p (RMacT (mac ln str))
smacdef   (p,str) ln = return $ T p (SMacDefT (macdef ln str))
rmacdef   (p,str) ln = return $ T p (RMacDefT (macdef ln str))
startcode (p,str) ln = return $ T p (IdT (take ln str))

isIdChar c = isAlphaNum c || c `elem` "_'"

extract ln str = take (ln-2) (tail str)
		
do_ech radix ln str = chr (parseInt radix str)

mac ln (_ : str) = take (ln-1) str

macdef ln (_ : str) = takeWhile (not.isSpace) str

esc (_ : x : _)  =
 case x of
   'a' -> '\a'
   'b' -> '\b'
   'f' -> '\f'
   'n' -> '\n'
   'r' -> '\r'
   't' -> '\t'
   'v' -> '\v'
   c   ->  c

parseInt :: Int -> String -> Int
parseInt radix ds = foldl1 (\n d -> n * radix + d) (map digitToInt ds)

-- In brace-delimited code, we have to be careful to match braces
-- within the code, but ignore braces inside strings and character
-- literals.  We do an approximate job (doing it properly requires
-- implementing a large chunk of the Haskell lexical syntax).

code (p,inp) len = go 1 ""
 where
  go 0 cs = return (T p (CodeT (reverse (tail cs))))
  go n cs = do
    c <- alexGetChar
    case c of
	Nothing  -> err
	Just c   -> case c of
			'{'  -> go (n+1) (c:cs)
			'}'  -> go (n-1) (c:cs)
			'\'' -> go_char n (c:cs)
			'\"' -> go_str n (c:cs) '\"'
			c    -> go n (c:cs)

	-- try to catch occurrences of ' within an identifier
  go_char n (c:cs) | isAlphaNum c = go n ('\'':c:cs)
  go_char n cs = go_str n cs '\''

  go_str n cs end = do
    c <- alexGetChar
    case c of
	Nothing -> err
	Just c
	  | c == end  -> go n (c:cs)
	  | otherwise -> 
		case c of
		   '\\' -> do
			d <- alexGetChar
			case d of
			  Nothing -> err
			  Just d  -> go_str n (d:c:cs) end
		   c -> go_str n (c:cs) end
				  

alexEOF (p,"")   = return (T p EOFT)
alexEOF (p,rest) = err

err = error "lexical error" -- TODO

lexer :: String -> [Token]
lexer str = runAlex str $ do
  let loop = do tok <- alexScan; 
		case tok of
		  T _ EOFT -> return []
		  _ -> do toks <- loop
			  return (tok:toks)
  loop  
}
