{------------------------------------------------------------------------------
				     Alex

`main'/`alex' is the lexical analyser generator.  It reads and parses the lx
program from its input file and places a Haskell module in the output file.
The function exported by the output module may be used with the packages in the
`Scan' module to build parsers.

The `Sort' and `Map' modules contain generic utilies for sorting and
maintaining finite maps.  `Scan' contains basic definitions and utilities for
scanning the source script.  `RExp' defines the abstract syntax of regular
expression and scanners.  `DFA' provides `scanner2dfa' for converting scanners
to DFAs.

Chris Dornan, Aug-95, 10-Jul-96, 29-Sep-97, 11-Apr-00
------------------------------------------------------------------------------}

module Main where

import Char
import System
import Sort
import Map
import Alex
import RExp
import DFA
import Scan


infixr 1  ?
infixl 2  &, &=, =&
infixr 3  &#
infix  3  #, #=
infixl 4  .|
infix  4  .#


-- `main' decodes the command line arguments and calls `alex'.  

main:: IO ()
main =	getArgs							>>= \args ->
	check_flags args					>>
	case args of
	  [fn] -> check_lit fn >>= \(lit,fn') -> alex lit fn fn'
	  [fn,fn'] -> check_lit fn >>= \(lit,_) -> alex lit fn fn'
	  _ -> usage
	where
	check_flags (('-':_):_) = usage
	check_flags _ = return ()

	check_lit fn = 
		case reverse fn of
		  'x':'.':rbs -> return (False,ext rbs)
		  'x':'l':'.':rbs -> return (True,ext rbs)
		  _ -> error (fn ++ ext_msg)

	usage = getProgName					>>= \prog ->
		error (vrn_ln ++ usg_ln prog)
	
	vrn_ln = "Alex 1.1 by Chris Dornan\n\n"

	usg_ln prog = "usage: " ++ prog ++ " src_fn [tgt_fn]\n"

	ext rbs = reverse ('s':'h':'.':rbs)

	ext_msg = ": only .x and .lx extensions are recognised\n"


-- The first argument to `lx' is true if the source file is a literate script,
-- the second argument the input file name and the third argument the output
-- file name.
--  
-- The input is preprocessed and parsed to produce a list of scanners and the
-- header text.  The header text contains all the code scraps specified in the
-- input, packaged as a function that takes the indentation string -- 2 spaces
-- for literate scripts, 0 otherwise -- and returns the header text as ShowS;
-- see the `shows' prelude function for details of this technique.  The names
-- of all the start codes are extracted and assigned unique positive integers
-- (with the `encode_start_codes' function), returning the scanners updated
-- with the assignemnts and the encodings of the assignemnts to be placed on
-- the output.  Finally `encode_dfa' is used to convert each of the scanners to
-- a DFA, dump it and encode it on the output; if required, `encode_assocs' is
-- used to dump the association list of tokens and action functions.

alex:: Bool -> String -> String -> IO ()
alex lit src tgt =
	readFile src						>>= \prg ->
	case parse_lx(pp prg) of
	  Right lp ->
		writeFile tgt (hdr ind (sc_hdr (foldr fmt "" (zip [0..] l))))
		where
		(LP hdr l,sc_hdr) = encode_start_codes ind lp
	  Left err -> error (src ++ ": " ++ err)
	where
	fmt (n,(fn,Nothing,scr)) tl = encode_dfa ind n fn scr tl
	fmt (n,(fn,Just aln,scr)) tl =
			encode_assocs ind aln scr (encode_dfa ind n fn scr tl)

	pp = if lit then literate else id

	ind = if lit then "  " else ""


encode_start_codes:: String -> LxScript -> (LxScript,ShowS)
encode_start_codes ind (LP hdr l) = (LP hdr l',sc_hdr)
	where
	l' = [(x,y,[(nm,mk_re_ctx re_ctx)| (nm,re_ctx)<-scr])| (x,y,scr)<-l]

	mk_re_ctx (RECtx scs lc re rc) = RECtx (map mk_sc scs) lc re rc

	mk_sc (nm,_) = (nm,if nm=="0" then 0 else app mp nm)


	sc_hdr tl =
		case al of
		  [] -> tl
		  (nm,_):rst -> "\n" ++ ind ++ nm ++ foldr f t rst
			where
			f (nm, _) t = "," ++ nm ++ t
			t = " :: Int\n" ++ foldr fmt_sc tl al
		where
		fmt_sc (nm,sc) t = ind ++ nm ++ " = " ++ show sc ++ "\n" ++ t


	mp = empty (<) <+> al

	al = zip (nub' (<=) nms) [1..]

	nms = [nm| (_,_,scr)<-l, (_,RECtx scs _ _ _)<-scr,
						(nm,_)<-scs, nm/="0"]


encode_assocs:: String -> String -> Scanner -> ShowS
encode_assocs ind al_nm scr tl =
			"\n" ++ ind ++ al_nm ++ " = " ++ al_str ++ "\n" ++ tl
	where
	al_str =
		if null ids
		   then "[]"
		   else "[" ++ foldr fmt (fmt'(last ids)++"]") (init ids)

	fmt ent tl = fmt' ent ++ ","++tl

	fmt' ide = "("++show ide++","++ide++")"

	ids = nub' (<=) [ide| (ide@(_:_),_)<-scr]


-- In addition to the function name, scanner and tail of the output, this
-- function takes a sequence number to generate names for top-level supporting
-- definitions that will not clash with those generated for other scanners.
-- Ideally, the supporting definitions would not be present but optimising
-- compilers tend to get into a strop when presented with giant top-level
-- definitions.  See the DFA section of the Scan module for details of the
-- `DFA' and `DFADump' types.

encode_dfa:: String -> Int -> String -> Scanner -> ShowS
encode_dfa ind n func_nm scr tl =
			"\n" ++ decl ++ defn ++ sub_defns ++ "\n" ++ tl
	where
	decl	= ind ++ func_nm ++ " :: " ++ ls_tp ++ "\n"
	defn	= ind ++ func_nm ++ " = " ++ lst_txt(length dump) ++ "\n"

	lst_txt 0 = "[]"
	lst_txt (n+1) = "[" ++ x_nm 0 ++ foldr f "]" [1..n]
		where
		f i str = "," ++ x_nm i ++ str

	sub_defns = foldr pr "" (zip [0..] dump)

	pr (i,e) t =
		ind ++ x_nm i ++ " :: " ++ nd_tp ++ "\n" ++
		ind ++ x_nm i ++ " = " ++ show e ++ "\n" ++ t

	dump = dump_dfa(scanner2dfa scr)

	ls_tp = "[" ++ nd_tp ++ "]"
	nd_tp =	"(Bool,\
		\ [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),\
								\Maybe Int)],\
		\ Int,\
		\ ((Char,Char),[(Char,Int)]))"

	x_nm i = "lx__" ++ shows i ('_':n_str)

	n_str = show n



{------------------------------------------------------------------------------
				 The lx Parser
------------------------------------------------------------------------------}



-- The `Parser' type is a specialisation of the general purpose `Psr' type
-- defined in the `Parsing Toolkit' section below.  The environment, tokens and
-- error parameters are specialised to `LxEnv', `Token' and `LxErr',
-- respectively.

type Parser a = Psr LxEnv Token LxErr a


-- The parser may fail with a syntax error or an undefined macro error.  Both
-- record the position of the error but `MacroLE' gives the name of the
-- offending macro too.

data LxErr = SyntaxLE Posn | MacroLE Posn String


-- The environment gives the bindings for the regular expression macros and the
-- set macros.  `empty_ev' defines the empty environment and `ext_ev' extends
-- them.

data LxEnv = LEv (Map Char RExp) (Map Char Set)

data MacDef = SMac Char (Char->Bool) | RMac Char RExp

empty_ev:: LxEnv
empty_ev = LEv (empty (<)) (empty (<))

ext_ev:: MacDef -> LxEnv -> LxEnv
ext_ev (SMac ch p) (LEv rmp smp) = LEv rmp (ins ch p smp)
ext_ev (RMac ch e) (LEv rmp smp) = LEv (ins ch e rmp) smp


-- `parse_lx' is the lx parser.  It will return either an error message or a
-- `LxScript'.  Notice the way it appends the input to the standard macros and
-- subtracts one from the line number in the diagnostics (the standard macros
-- are contained on one line).

parse_lx:: String -> Either String LxScript
parse_lx inp =
	case parse lx_p token_err empty_ev (scan_lx inp') of
	  Right x -> Right x
	  Left (SyntaxLE (Pn a n c)) -> Left (syn_fmt a n c)
	  Left (MacroLE (Pn a n c) nm) -> Left (mac_fmt a n c nm)
	where
	inp' = standard_macros ++ inp

	token_err tkn = SyntaxLE (token_pos tkn)

	syn_fmt a n c =
		posn_fmt n c ++ ": syntax error\n    " ++ frag a

	mac_fmt a n c nm =
		posn_fmt n c ++ ": macro " ++ nm ++ " is not defined\n"

	posn_fmt n c = "line " ++ show (n-1) ++ ", column " ++ show c

	frag a = "\"" ++ takeWhile ('\n'/=) (drop a inp') ++ "\"\n"

standard_macros:: String
standard_macros =
	"{^a=^7}{^b=^8}{^t=^9}{^n=^10}{^v=^11}{^f=^12}{^r=^13}\
	\{^w=[^t^n^v^f^r^ ]}{^p=^32-^126}{.=^0-^255#^n}\n"



{------------------------------------------------------------------------------
				The Lx Scanner
------------------------------------------------------------------------------}



-- This section defines the `Token' type and the accompanying scanner,
-- `scan_lx'.  It combines the `lx_lx' DFA dump with sateful actions to produce
-- a `ScanS' scanner.  `lx_lx' will in the first instance come from the
-- bootstrapping section; once the bootstrap version has been built, the
-- specification in `lx.lx' will be compiled and imported and the bootstrapping
-- section commented out.  The actions are given here in order to avoid
-- duplicating them in the bootstrapping section and the lx specification.
--  
-- The state of the scanner is used to accumulate the code scraps.  The stae
-- takes the form `String->ShowS' where the argument string represents the
-- indentation text (which differs between literate and ordinary scripts) and
-- returns a `ShowS' function; see the `shows' prelude function for details of
-- this construction.  This technique may be a tad leaky so it may change after
-- profiling.

data Token = LxT Posn LxTkn

data LxTkn =
	BndT | PrdT | ZerT | IdeT String | TknT String |
	SpeT Char | SMacT Char | RMacT Char | ChT Char | QuoT [Char] |
	ErrT | EofT (String->ShowS)

token_pos:: Token -> Posn
token_pos (LxT p _) = p


scan_lx:: String -> [Token]
scan_lx inp = gscan lx_scan (\ind->id) inp

lx_scan:: GScan (String->ShowS) [Token]
lx_scan = load_gscan (token_actions,stop_action) lx_lx
	where
	token_actions =
		[ ("code", code						)
		,f("prd",  \p ln str -> LxT p  PrdT			)
		,f("bnd",  \p ln str -> LxT p  BndT			)
		,f("zero", \p ln str -> LxT p  ZerT			)
		,f("tkn",  \p ln str -> LxT p (TknT  (extract ln str))	)
		,f("ide",  \p ln str -> LxT p (IdeT  (extract ln str))	)
		,f("spe",  \p ln str -> LxT p (SpeT  (head str))	)
		,f("ch",   \p ln str -> LxT p (ChT   (head str))	)
		,f("ech",  \p ln str -> LxT p (ChT   (str!!1))		)
		,f("cch",  \p ln str -> LxT p (ChT   (cch ln str))	)
		,f("smac", \p ln str -> LxT p (SMacT (smac str))	)
		,f("rmac", \p ln str -> LxT p (RMacT (str!!1))		)
		,f("quot", \p ln str -> LxT p (QuoT  (extract ln str))	)
		]
		where
		f (nm,ac) = (nm, mk_act ac)

		mk_act ac = \p _ str len cont st->ac p len str:cont st
	
		code p _ str ln cont (sc,hdr) = cont (sc,hdr')
			where
			hdr' ind tl =
				case str' of
				  '\n':_ -> hdr ind (skip (ln'-1) str' bfrag)
				  _ -> hdr ind ('\n':ind++skip ln' str' lfrag)
				where
				bfrag (' ':_) =      tl
				bfrag _       = '\n':tl
		
				lfrag _       = '\n':tl

			ln' = ln - 4 - length ldr
		
			(ldr,str') =  span (`elem` " \t\v\f") (drop 2 str)
		
			skip (n+1) (c:rst) f = c:skip n rst f
			skip _     rst     f = f rst
	
		extract ln str = take (ln-2) (tail str)
		
		cch ln str = chr(str2int(take (ln-1) (tail str)))
		
		smac ('.':_) = '.'
		smac str     = str!!1

	stop_action p _ "" (_,hdr) = [LxT p (EofT hdr)]
	stop_action p _ _ _ = [LxT p ErrT]



{------------------------------------------------------------------------------
			       Bootstrapping lx
------------------------------------------------------------------------------}



{-
-- This section contains a dump of the scanner used to tokenise lx scripts.  It
-- is used to bootstrap `lx' scripts so the normal lx script is encoded
-- directly in the abstract syntax.  The resulting lx image is used to generate
-- the final dump in module `Lx_lx', the module is compiled and `lx_lx' is
-- imported and this section is commented out.
--  
-- The literate preprocessor is defined with the error function as literate
-- scripts are not supported in the bootstrap version.

lx_lx:: DFADump
lx_lx = dump_dfa(scanner2dfa lx_scr)

lx_scr:: Scanner
lx_scr =
	[f("",      Plus(Ch (`elem` "\t\n\f "))                             )
	,f("",      ch '-' :%% ch '-' :%% Star dot	                    )
	,f("code",  code_block						    )
	,f("code",  code_line						    )
	,f("zero",  ch '"' :%% ch '0' :%% ch '"'			    )
	,f("ide",   ch '"' :%% ide :%% ch '"'				    )
	,f("tkn",   ch '<' :%% Ques ide :%% ch '>'			    )
	,f("bnd",   ch ':' :%% ch '-'		                            )
	,f("prd",   ch ':' :%% ch ':' :%% ch '='                            )
	,f("spe",   Ch (`elem` "{=}:\\/|*+?,$()~#[]-")                      )
	,f("ch",    Ch isAlphaDigit                                         )
	,f("ech",   ch '^' :%% Ch notAlphaDigit                             )
	,f("cch",   ch '^' :%% Plus(Ch isDigit)                             )
	,f("smac",  ch '^' :%% Ch isAlpha  :|  ch '.'                       )
	,f("rmac",  ch '%' :%% Ch isAlpha                                   )
	,f("quot",  ch '`' :%% Plus(Ch quotable) :%% ch '\''                )
	]
	where
	f (nm,re) = (nm,RECtx [] Nothing re Nothing)

	ch c = Ch (c==)

	code_block = ch '%' :%% ch '{' :%% ch '\n' :%% 
			Star cb_body :%% ch '\n' :%% ch '%' :%% ch '}' 

	code_line = ch '%' :%% ch '{' :%% Star cl_body :%% ch '%' :%% ch '}'

	cb_body = dot :| ch '\n' :%% (Ch notPer :| ch '%' :%% Ch notBrace)

	cl_body = Ch notPer' :| ch '%' :%% Ch notBrace'

	ide = Ch isAlpha :%% Star(Ch (isAlpha .| isDigit .| (`elem` "_\'")))

	dot = Ch ('\n'/=)

	notPer = (/='%')
	notBrace = (/='}')
	notPer' = (`notElem` "%\n")
	notBrace' = (`notElem` "}\n")
	isAlphaDigit = isAlpha .| isDigit
	notAlphaDigit = isPrint .# isAlphaDigit
	quotable = isPrint .# (=='\'')

literate:: String -> String
literate inp = error "literate scripts not available in bootstrap\n"
-}



{------------------------------------------------------------------------------
				The Lx Grammar
------------------------------------------------------------------------------}



-- This section contains the body of the lx parser.  The main parser, `lx_p',
-- produces `LxScript' values, consisting of the amalgamation of code scraps
-- found in the input and the scanners parsed.  Each scanner comes with the
-- name that the dump should be bound to in the resulting program and the, if
-- an association list is needed, the name it should take.
--  
-- The operator grammar is given as a recursive decent grammar rather than with
-- the more efficient recursive ascent technique as it is more natural; speed
-- is not an issue for this parser.

data LxScript = LP (String->ShowS) [(String,Maybe String,Scanner)]


-- The grammar is expressed as a deterministic combinator parser.  The scheme
-- is based on the elegant one devised by Ian Holyer for the Brisk parser.  The
-- principal operators are `#', `&' and `?'.  `#' separates actions from
-- parsers, `&' sequentially composes parsers and `?' is used to combine
-- alternative parsers.  For example, the parser
--  
-- 		    foldl (:%%) #  rexp1_p & many rexp1_p
--  
-- will parse a regular expression followed by a sequence of zero or more
-- regular expressions, `many' being a parser combinator of type 
--  
-- 	Parser a -> Parser [a]
--  
-- (Actually it is a bit more general, operating over the generic `Psr' type.)
-- As `rexp1_p' is of type `Parser RExp', `many rexp1_p' will be of type
-- `Parser [RExp]'.  The action `foldl (:%%)' is of type
-- `RExp -> [RExp] -> RExp', as required, as the result of the first parser
-- will be passed to its first argument and the second parser to its second
-- argument.  The result of the whole parser is thus `Parser RExp'.
--  
-- Often parsers will parse a terminal symbol returning the unit type.  The
-- `spe' parser is a good example.  It recognises `SymT c' tokens, taking the
-- character expected in its first argument and returning `()' if the
-- apropriate symbol is found, so `spe '='' will match `SpeT '='' tokens.  The
-- `()' returned by such a parser is not usually needed by the action so it can
-- be suppressed by using a `&=' operator to its left: 
--  
-- 			   SMac #  smac_tok &= spe '=' & set_p
--  
-- Here the `SMac' action will only see the results of the `smac_tok' and
-- `set_p' parsers.  Sometimes the action will ignore the parser to its right:
--
--			    Eps #= spe '$'
--  
-- At other times the argument to the left of an `&' needs to be discarded:
-- 
-- 				   spe '(' =& rexp_p &= spe ')'
--  
-- Alternatives can be expressed with the `?' operator.  It has the lowest
-- priority.  Both its arguments should be parsers of the same type:
--  
--			   SMac #  smac_tok &= spe '=' & set_p		      ?
-- 			   RMac #  rmac_tok &= spe '=' & rexp_p
--  
-- Occasionally, the result of a parser may need to be passed as an argument to
-- another parser:
--  
--				   macdef_p &# add_macro (macros_p psr)
--  
-- The result of the `macdef_p' parser is passed to the
-- `add_macro (macros_p psr)' as an extra argument.  The result of the whole
-- parser is given by the parser to the right of `&#'.


lx_p:: Parser LxScript
lx_p':: Parser [(String,Maybe String,Scanner)]
macros_p:: Parser a -> Parser a
macdef_p,macdef_p':: Parser MacDef
scanner_p:: Parser (String,Maybe String,Scanner)
def_p:: Parser (String,RExpCtx)
ctx_p:: Parser RExpCtx
ctx_p':: [(String,StartCode)] -> RExp -> Parser RExpCtx
sc_p:: Parser (String,StartCode)
rexp_p, rexp2_p, rexp1_p, rexp0_p:: Parser RExp
st_pl_qu_rg:: Parser (RExp->RExp)
range_p:: Parser (Int,Maybe(Maybe Int))
set_p, set0_p:: Parser (Char->Bool)


-- lx scripts


lx_p =			flip LP	#  lx_p' & eof_p

lx_p' =				   macros_p (many scanner_p)


-- macro definitions


macros_p psr = 			   macdef_p &# add_macro (macros_p psr)	      ?
				   psr

macdef_p =			   spe '{' =& macdef_p' &= spe '}'

macdef_p' =		   SMac #  smac_tok &= spe '=' & set_p		      ?
			   RMac #  rmac_tok &= spe '=' & rexp_p


-- scanners


scanner_p =		 triple	#  ide_p & poss (spe '/' =& ide_p)
				   	      &= bnd_p & macros_p(many def_p)

def_p =		           pair	#  tkn_p &= prd_p & macros_p ctx_p

ctx_p = 			   many (sc_p &= spe ':') &# 
						\scs -> rexp_p &# ctx_p' scs

ctx_p' scs re@(Ch st) =
	    RECtx scs (Just st)	#= spe '\\' & rexp_p &
					poss (spe '/' =& rexp_p)	      ?
	   RECtx scs Nothing re #  poss (spe '/' =& rexp_p)
ctx_p' scs re = 
	   RECtx scs Nothing re #  poss (spe '/' =& rexp_p)

sc_p =		  (\sc->(sc,0))	#  ide_p				      ?
			("0",0) #= zero_p

-- regular expressions


rexp_p = 	     foldl (:|)	#  rexp2_p & many (spe '|' =& rexp2_p)

rexp2_p = 	    foldl (:%%) #  rexp1_p & many rexp1_p

rexp1_p =    fold_mb (flip ($)) #  rexp0_p & poss st_pl_qu_rg


st_pl_qu_rg =		   Star #= spe '*' 				      ?
			   Plus #= spe '+' 				      ?
			   Ques #= spe '?'				      ?
		     repeat_rng #= spe '{' & range_p &= spe '}'

range_p =		   pair #  int_p & poss(spe ',' =& poss int_p)
				

rexp0_p =		    Eps #= spe '$'				      ?
				   rmacro_p				      ?
			     Ch #  set_p				      ?
				   spe '(' =& rexp_p &= spe ')'


-- character sets


set_p =		   fold_mb (.#) #  set0_p & poss (spe '#' =& set0_p)

set0_p =	     complement	#= spe '~' & set0_p & get_env		      ?
			mb_dash #  chr_tok & poss (spe '-' =& chr_tok)       ?
				   smacro_p				      ?
       foldl (.|) (const False) #= spe '[' & many set_p &= spe ']'	      ?
				   quot_p
	where
	mb_dash ch Nothing = (ch==)
	mb_dash ch (Just ch') = \ch''->ch<=ch'' && ch''<=ch'


repeat_rng:: (Int,Maybe(Maybe Int)) -> (RExp->RExp)
repeat_rng (n,Nothing) re = foldr (:%%) Eps (copy n re)
repeat_rng (n,Just Nothing) re = foldr (:%%) (Star re) (copy n re)
repeat_rng (n,Just (Just m)) re = intl :%% rst
	where
	intl = repeat_rng (n,Nothing) re
	rst = foldr (\re re'->Ques(re :%% re')) Eps (copy (m-n) re)


fold_mb:: (a->b->a) -> a -> Maybe b -> a
fold_mb f x Nothing = x
fold_mb f x (Just y) = f x y

complement:: Set -> LxEnv -> Set
complement p (LEv _ mp) = (dot_p .| nl_p) .# p
	where
	dot_p = app mp '.'
	nl_p  = app mp 'n'

(.|):: Set -> Set -> Set
(.|) p p' x = p x || p' x

(.#):: Set -> Set -> Set
(.#) p p' x = p x && not(p' x)




{------------------------------------------------------------------------------
				Atomic Parsers
------------------------------------------------------------------------------}



-- `add_macro' takes a parser and a macro definition and returns a parser that
-- parses in an environment extended with the macro definition.

add_macro:: Parser a -> MacDef -> Parser a
add_macro psr md = get_env &# \ev -> set_env (ext_ev md ev) psr


-- `macro_fail_p' fails with an undefined macro error.

macro_fail_p:: Posn -> String -> Parser a
macro_fail_p p nm = fail_p (MacroLE p nm)


-- All the followin parsers parse single tokens.  They use the `tok_p' parser
-- that takes a function `f' from the next token on the input to `Just a' if
-- the parser is to succeed with `a', `Nothing' if it should fail.

prd_p:: Parser ()
prd_p = tok_p f
	where
	f PrdT = Just ()
	f _ = Nothing

bnd_p:: Parser ()
bnd_p = tok_p f
	where
	f BndT = Just ()
	f _ = Nothing

ide_p:: Parser String
ide_p = tok_p f
	where
	f (IdeT str) = Just str
	f _ = Nothing

zero_p:: Parser ()
zero_p = tok_p f
	where
	f ZerT = Just ()
	f _ = Nothing

tkn_p:: Parser String
tkn_p = tok_p f
	where
	f (TknT str) = Just str
	f _ = Nothing

quot_p:: Parser (Char->Bool)
quot_p = tok_p f
	where
	f (QuoT chs) = Just (`elem` chs)
	f _ = Nothing

int_p:: Parser Int
int_p = tok_p f
	where
	f (ChT ch) | isDigit ch = Just (ord ch-ord '0')
	f _ = Nothing

smacro_p:: Parser (Char->Bool)
smacro_p =
	smac_tok' 					&# \(p,ch) ->
	get_env 					&# \(LEv _ mp) ->
	case app' mp ch of
	  Nothing -> macro_fail_p p nm
		where
		nm = if ch=='.' then "." else ['^',ch]
	  Just pr -> return_p pr

rmacro_p:: Parser RExp
rmacro_p =
	rmac_tok' 					&# \(p,ch) ->
	get_env 					&# \(LEv mp _) ->
	case app' mp ch of
	  Nothing -> macro_fail_p p ['%',ch]
	  Just re -> return_p re

spe:: Char -> Parser ()
spe ch = tok_p f
	where
	f (SpeT ch') | ch==ch' = Just ()
	f _ = Nothing

chr_tok:: Parser Char
chr_tok = tok_p f
	where
	f (ChT ch) = Just ch
	f _ = Nothing

smac_tok:: Parser Char
smac_tok = snd # smac_tok'

smac_tok':: Parser (Posn,Char)
smac_tok' = tok_p' f
	where
	f p (SMacT ch) = Just (p,ch)
	f _ _ = Nothing

rmac_tok:: Parser Char
rmac_tok = snd # rmac_tok'

rmac_tok':: Parser (Posn,Char)
rmac_tok' = tok_p' f
	where
	f p (RMacT ch) = Just (p,ch)
	f _ _ = Nothing

eof_p:: Parser (String->ShowS)
eof_p = tok_p f
	where
	f (EofT hdr) = Just hdr
	f _ = Nothing

tok_p:: (LxTkn->Maybe a) -> Parser a
tok_p f = tok_p' (const f)

tok_p':: (Posn->LxTkn->Maybe a) -> Parser a
tok_p' f = tok' (SyntaxLE eof_pos) g
	where
	g (LxT p tk) =
		case f p tk of
		  Nothing -> Left (SyntaxLE p)
		  Just r -> Right r



{------------------------------------------------------------------------------
				Parsing Toolkit
------------------------------------------------------------------------------}



-- This is a generic parsing toolkit.  `Psr e t d a' is a parser with
-- environmant `e', token stream `s', diagnostics `d' that returns `a' values.
-- See the comments at the top of the Lx Grammar section for an overview of
-- the `Psr' operators.


many:: Psr e t d a -> Psr e t d [a]
many g = (:) # g & many g ? return_p []

many_1:: Psr e t d a -> Psr e t d [a]
many_1 g = (:) # g & many g

poss:: Psr e t d a -> Psr e t d (Maybe a)
poss g = Just # g ? return_p Nothing

(&):: Psr e t d (a->a') -> Psr e t d a -> Psr e t d a'
(&) p p' = p &# \f -> f # p'

(&=):: Psr e t d a -> Psr e t d () -> Psr e t d a
(&=) p p' = p &# \r -> p' &# \_ -> return_p r

(=&):: Psr e t d () -> Psr e t d a -> Psr e t d a
(=&) p p' = p &# \_ -> p'

(#=):: a -> Psr e t d () -> Psr e t d a
(#=) x psr = const x # psr


type Psr e t d a = e -> [t] -> (Either d a,Bool,[t])
-- 	in parse, return_p, fail_p, get_env, set_env, (#), (&#), (?), tok'

-- The `Psr' is implemented as an error-handling state-transformer with an
-- environmenent, where the state is the list of tokens remaining to be parsed.
-- The `Bool' component records whether the token stream was modified or not;
-- it is used by `?' to determine whether the alternative parser should be
-- tried if left-hand one fails (it won't if the token stream has been
-- modified).

parse:: Psr e t d a -> (t->d) -> e -> [t] -> Either d a
parse psr f ev tks =
	case psr ev tks of
	  (Right x,_,[]) -> Right x
	  (Left dg,_,tks) -> Left dg
	  (Right _,_,(t:_)) -> Left (f t)

return_p:: a -> Psr e t d a
return_p x ev tks = (Right x,False,tks)

fail_p:: d -> Psr e t d a
fail_p err ev tks = (Left err,False,tks)

get_env:: Psr e t d e
get_env ev tks = (Right ev,False,tks)

set_env:: e -> Psr e t d a -> Psr e' t d a
set_env ev psr _ tks = psr ev tks

(#):: (a->a') -> Psr e t d a -> Psr e t d a'
(#) f psr ev tks =
	case psr ev tks of
	  (Right x,b,tks') -> (Right(f x),b,tks')
	  (Left dg,b,tks') -> (Left dg,b,tks')

(&#):: Psr e t d a -> (a->Psr e t d a') -> Psr e t d a'
(&#) psr f_psr ev tks =
	case psr ev tks of
	  (Right x,b,tks') ->
		case f_psr x ev tks' of
		  (Right y,b',tks'') -> (Right y,b||b',tks'')
		  (Left dg,b',tks'') -> (Left dg,b||b',tks'')
	  (Left dg,b,tks') -> (Left dg,b,tks')

(?):: Psr e t d a -> Psr e t d a -> Psr e t d a
(?) psr psr' ev tks =
	case psr ev tks of
	  (Left _,False,tks) -> psr' ev tks
	  res -> res

tok':: d -> (t->Either d a) -> Psr e t d a
tok' d f ev [] = (Left d,False,[])
tok' d f ev tks@(tk:tks') =
	case f tk of
	  r@(Right _) -> (r,True,tks')
	  r@(Left _) -> (r,False,tks)




{------------------------------------------------------------------------------
			       General Utilities
------------------------------------------------------------------------------}



str2int:: String -> Int
str2int = foldl (\n d-> n*10+ord d-ord '0') 0

copy:: Int -> a -> [a] 
copy (n+1) x = x:take n xs where xs = x:xs
copy m x = []

pair:: a -> b -> (a,b)
pair x y = (x,y)

triple:: a -> b -> c -> (a,b,c)
triple x y z = (x,y,z)
