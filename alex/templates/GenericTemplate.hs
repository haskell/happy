
-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data Posn = Pn !Int !Int !Int
	deriving (Eq,Show)

start_pos:: Posn
start_pos = Pn 0 1 1

eof_pos:: Posn
eof_pos = Pn (-1) (-1) (-1)

move_pos:: Posn -> Char -> Posn
move_pos (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
move_pos (Pn a l c) '\n' = Pn (a+1) (l+1)   1
move_pos (Pn a l c) _    = Pn (a+1)  l     (c+1)


type GStopAction s r = Posn -> Char -> String -> (StartCode,s) -> r

--gscan :: GStopAction s r -> s -> String -> r
--gscan':: GStopAction s r -> Posn -> Char -> String -> (StartCode,s) -> r

gscan stop s inp = gscan' stop start_pos '\n' inp (0,s)

gscan' stop p c inp sc_s =
	case scan_token sc_s p c inp of
	  Nothing -> stop p c inp sc_s
	  Just (p',c',inp',len,Acc _ t_a _ _) ->
		t_a p c inp len (gscan' stop p' c' inp') sc_s

{------------------------------------------------------------------------------
				  scan_token
------------------------------------------------------------------------------}

-- `scan_token' picks out the next token from the input.  It takes the DFA and
-- the usual parameters and returns the `Accept' structure associated with the
-- highest priority token matching the longest input sequence, nothing if no
-- token matches.  Associated with `Accept' in `Sv' is the length of the token
-- as well as the position, previous character and remaining input at the end
-- of accepted token (i.e., the start of the next token).

type Sv t = (Posn,Char,String,Int,Accept t)

--scan_token:: (StartCode,s) -> Posn -> Char -> String -> Maybe (Sv f)
scan_token (startcode,_) p c inp = scan_tkn p c inp 0 startcode Nothing
		-- the startcode is the initial state

-- This function performs most of the work of `scan_token'.  It pushes the
-- input through the DFA, remembering the accepting states it encounters on a
-- stack.  No context is checked here.  A space leak could result from a long
-- token with many valid prefixes, leading to a large stack.  This space leak
-- is avoided in most cases by discarding the stack if an unconditional state
-- is pushed on (no state below an unconditional state will be needed).

--scan_tkn:: Posn -> Char -> String -> Int -> SNum -> Maybe (Sv f) -> Maybe (Sv f)
scan_tkn p c inp      len (-1) stk = stk  	-- error or finished
scan_tkn p c []       len s    stk = stk	-- end of input (correct?)
scan_tkn p c inp@(c':inp') len s    stk =
#ifdef ALEX_DEBUG
  trace ("State: " ++ show s ++ ", char: " ++ show c') $
#endif
  stk' `seq` scan_tkn p' c' inp' (len+1) s' stk'
  where
	p' = move_pos p c'

	base   = alex_base!s
	offset = base + ord c'
	
	s' = if offset >= 0 && alex_check!offset == ord c'
		then alex_table!offset
		else alex_deflt!s

	svs  =	[ (p,c,inp,len,acc)
		| acc <- alex_accept!s, 
		  check_ctx acc ]

	stk' = case svs of
		[]     -> stk
		(x:xs) -> Just x

	check_ctx (Acc _ _ lctx rctx) = chk_lctx lctx && chk_rctx rctx
		where
		chk_lctx Nothing   = True
		chk_lctx (Just st) = st!c

		chk_rctx Nothing   = True
		chk_rctx (Just sn) = case scan_tkn p c inp 0 sn Nothing of
					Nothing -> False
					Just _  -> True
			-- TODO: there's no need to find the longest
			-- match when checking the right context, just
			-- the first match will do.

type SNum = Int

data Accept a 
  = Acc { accPrio       :: Int,
	  accAction     :: a,
	  accLeftCtx    :: Maybe (Array Char Bool),
	  accRightCtx   :: Maybe SNum
    }

type StartCode = Int
