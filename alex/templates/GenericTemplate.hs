#ifdef ALEX_GHC
#define ILIT(n) n#
#define IBOX(n) (I# (n))
#define FAST_INT Int#
#define LT(n,m) (n <# m)
#define GTE(n,m) (n >=# m)
#define EQ(n,m) (n ==# m)
#define PLUS(n,m) (n +# m)
#define MINUS(n,m) (n -# m)
#define TIMES(n,m) (n *# m)
#define NEGATE(n) (negateInt# (n))
#define IF_GHC(x) (x)
#else
#define ILIT(n) (n)
#define IBOX(n) (n)
#define FAST_INT Int
#define LT(n,m) (n < m)
#define GTE(n,m) (n >= m)
#define EQ(n,m) (n == m)
#define PLUS(n,m) (n + m)
#define MINUS(n,m) (n - m)
#define TIMES(n,m) (n * m)
#define NEGATE(n) (negate (n))
#define IF_GHC(x)
#endif

#ifdef ALEX_GHC
#undef __GLASGOW_HASKELL__
#define ALEX_IF_GHC_GT_500 #if __GLASGOW_HASKELL__ > 500
#define ALEX_IF_GHC_GE_503 #if __GLASGOW_HASKELL__ >= 503
#define ALEX_ELIF_GHC_500 #elif __GLASGOW_HASKELL__ == 500
#define ALEX_ELSE #else
#define ALEX_ENDIF #endif
#endif

#ifdef ALEX_GHC
data AlexAddr = AlexA# Addr#

indexShortOffAddr (AlexA# arr) off =
ALEX_IF_GHC_GT_500
	narrow16Int# i
ALEX_ELIF_GHC_500
	intToInt16# i
ALEX_ELSE
	(i `iShiftL#` 16#) `iShiftRA#` 16#
ALEX_ENDIF
  where
ALEX_IF_GHC_GE_503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
ALEX_ELSE
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
ALEX_ENDIF
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#
#else
indexShortOffAddr arr off = arr ! off
#endif

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
scan_token (IBOX(startcode),_) p c inp
 = scan_tkn p c c inp ILIT(0) startcode Nothing
		-- the startcode is the initial state

-- This function performs most of the work of `scan_token'.  It pushes
-- the input through the DFA, remembering the most recent accepting
-- states it encountered.

--scan_tkn:: Posn -> Char -> String -> Int -> SNum -> Maybe (Sv f) -> Maybe (Sv f)
scan_tkn _ _ _ _ _ (ILIT(-1)) stk = 
#ifdef ALEX_DEBUG
	(case stk of
	  Nothing -> trace "error"
	  Just _  -> trace "accept")
#endif
	stk  	-- error or finished

scan_tkn p lc c inp len s stk =
	  -- lc is the left-context char, c is the previous char
  case inp of
    [] -> 
#ifdef ALEX_DEBUG
	trace "EOF"
#endif
	stk'	-- end of input

    (c':inp') -> 
#ifdef ALEX_DEBUG
	trace ("State: " ++ show IBOX(s) ++ ", char: " ++ show c') $
#endif
	stk' `seq` scan_tkn p' lc c' inp' PLUS(len,ILIT(1)) s' stk'
      	where
		p' = move_pos p c'

		base   = indexShortOffAddr alex_base s
		IBOX(ord_c) = ord c'
		offset = PLUS(base,ord_c)
		check  = indexShortOffAddr alex_check offset
	
		s' = 
		     if GTE(offset,ILIT(0)) && EQ(check,ord_c)
			then indexShortOffAddr alex_table offset
			else indexShortOffAddr alex_deflt s
   where
	svs  =	[ (p,c,inp,IBOX(len),acc)
		| acc <- alex_accept!IBOX(s),
		  check_ctx acc ]

	stk' = case svs of
		[]     -> stk
		(x:xs) -> Just x

	check_ctx (Acc _ _ lctx rctx) = chk_lctx lctx && chk_rctx rctx
		where
		chk_lctx Nothing   = True
		chk_lctx (Just st) = st ! lc

		chk_rctx Nothing   = True
		chk_rctx (Just (IBOX(sn))) = 
			case scan_tkn p c c inp ILIT(0) sn Nothing of
				Nothing -> False
				Just _  -> True
			-- TODO: there's no need to find the longest
			-- match when checking the right context, just
			-- the first match will do.

type SNum = Int

data Accept a = Acc Int a (Maybe (Array Char Bool)) (Maybe SNum)

type StartCode = Int

