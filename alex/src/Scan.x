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

%{
module Scan(lx_lx,literate) where

import Alex
%}


{ ^s = ^w#^n	  }					-- spaces + tabs, etc
{ ^d = 0-9        }					-- digits
{ ^a = a-z        }					-- lower-case alphas
{ ^A = A-Z        }					-- upper-case alphas
{ ^l = [^a^A]     }					-- alpha characters
{ ^i = [^l^d^_^'] }					-- identifier trailer
	      
"lx_lx" :-

  <>     ::=  ^w+ 					-- white space
  <>     ::=  ^-^-.*					-- comments
  <code> ::= 						-- code scraps:
	^%^{ (.#^%|^%.#^})* ^%^}			--   single-line scraps
	|  ^%^{^s*^n (((.#^%.*)?|^%(.#^}.*)?)^n)* ^%^}	--   multi-line scraps
	|  ^%^{^s*^n					--   multi-line scraps
	      (((.#^ .*)?|^ (.#^%.*)?|^ ^%(.#^}.*)?)^n)*--	(literate
			^ ^%^}				--	 scripts)
  <zero> ::=  ^" 0 ^"					-- "0" start code
  <ide>  ::=  ^" ^a^i* ^"				-- function identifier
  <tkn>  ::=  ^< (^a^i*)? ^>				-- token identifier
  <bnd>  ::=  ^:^-					-- ":-"
  <prd>  ::=  ^:^:^=					-- "::="
  <spe>  ::=  `{=}:\/|*+?,$()~#[]-'			-- specials
  <ch>   ::=  [^l^d]					-- letter or digit
  <ech>  ::=  ^^ ^p#[^l^d]				-- escaped symbols
  <cch>  ::=  ^^ ^d{1,3}				-- character codes
  <smac> ::=  ^^ ^l | ^.				-- set macros
  <rmac> ::=  ^% ^l					-- rexp macros
  <quot> ::=  ^` ^p#^'+ ^'				-- quoted sets




"lit_lx"/"lit_acts":-


  { %b = ^n^s*			  }
  { %s = ^n^>.*			  }
  { %c = ^n(.#[^>^s].*|^s+.#^s.*) }

  <scrap>   ::= %b%s+
  <comment> ::= %b%c*


%{
scrap :: GTokenAction () String
scrap _ _ inp len cont st = strip len inp
	where
	strip 0 _ = cont st
	strip (n+1) (c:rst) =
		if c=='\n'
		   then '\n':strip_nl n rst
		   else c:strip n rst
	strip _ _ = error "scrap"

	strip_nl (n+1) ('>':rst) = ' ':strip n rst
	strip_nl n rst = strip n rst

comment :: GTokenAction () String
comment _ _ inp len cont st = strip len inp
	where
	strip 0 _ = cont st
	strip (n+1) (c:rst) = if c=='\n' then c:strip n rst else strip n rst
	strip _ _ = error "comment"


literate:: String -> String
literate inp = drop 2 (gscan lit_scan () ('\n':'\n':inp))

lit_scan:: GScan () String
lit_scan = load_gscan (lit_acts,stop_act) lit_lx
	where
	stop_act p _ "" st = []
	stop_act p _ _  _  = error (msg ++ loc p ++ "\n")

	msg  = "literate preprocessing error at "

	loc (Pn _ l c) = "line " ++ show(l-2) ++ ", column " ++ show c
%}
