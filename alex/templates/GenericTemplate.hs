-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- (c) Chris Dornan and Simon Marlow 2003

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- The Alex monad
--
-- Compile with -funbox-strict-fields for best results!

data AlexState = AlexState {
	alex_pos :: !AlexPosn,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_chr :: !Char,	-- the character before the input
	alex_scd :: !Int 	-- the current startcode
    }

type AlexInput = (AlexPosn,String)

runAlex :: String -> Alex a -> a
runAlex input (Alex f) 
   = snd (f (AlexState {alex_pos = alexStartPos,
 			alex_inp = input,	
			alex_chr = '\n',
			alex_scd = 0}))

--TODO include error support
newtype Alex a = Alex { unAlex :: AlexState -> (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> (s,a)

alexGetChar :: Alex (Maybe Char)
alexGetChar = Alex $ \st@AlexState{ alex_inp = inp, alex_pos = pos } ->
		case inp of
		   [] -> (st, Nothing)
		   (c:s) -> (st {alex_inp = s, 
				 alex_pos = alexMove pos c}, 
			     Just c)

alexGetInput :: Alex AlexInput
alexGetInput = Alex $ \s@AlexState{alex_pos=pos,alex_inp=inp} -> (s, (pos,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,inp) = Alex $ \s -> (s{alex_pos=pos,alex_inp=inp}, ())

alexGetLCtx :: Alex Char
alexGetLCtx = Alex $ \s@AlexState{alex_chr=c} -> (s, c)

alexSetLCtx :: Char -> Alex ()
alexSetLCtx c = Alex $ \s -> (s{alex_chr=c}, ())

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> (s{alex_scd=sc}, ())

-- -----------------------------------------------------------------------------
-- Useful token actions

#ifdef ALEX_MONAD
-- just ignore this token and scan another one
skip input len = alexScan

-- ignore this token, but set the start code to a new value
begin code input len = do alexSetStartCode code; alexScan

-- perform an action for this token, and set the start code to a new value
(token `andBegin` code) input len = do alexSetStartCode code; token input len
#endif

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

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

{-# INLINE alexIndexShortOffAddr #-}
alexIndexShortOffAddr (AlexA# arr) off =
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
alexIndexShortOffAddr arr off = arr ! off
#endif

-- -----------------------------------------------------------------------------
-- Main lexing routines

#if ALEX_MONAD

-- alexScan :: some a . Alex a
alexScan = do
  IBOX(startcode) <- alexGetStartCode  -- the startcode is the initial state
  cur_input <- alexGetInput
  c  <- alexGetLCtx 
  r <- alex_scan_tkn c c ILIT(0) startcode AlexNone
  case r of
    AlexNone ->
#ifdef ALEX_DEBUG
	trace ("Error, or end of input.") $ do
#endif
	alexEOF cur_input
    AlexLastAcc k c input len -> do
#ifdef ALEX_DEBUG
	trace ("Accept.") $ do
#endif
	alexSetInput input
	alexSetLCtx c
	k cur_input len

#else
-- We can provide the old continuation-based interface built on top of
-- the monad internals, which are hidden from the user.

alexGScan stop state inp = alex_gscan stop alexStartPos '\n' inp (0,state)

alex_gscan stop p c inp (IBOX(sc),state) =
  case unAlex (alex_scan_tkn c c ILIT(0) sc AlexNone) 
		(AlexState p inp c IBOX(sc)) of 
    (_, r) ->
	case r of
	   AlexNone -> stop p c inp (IBOX(sc),state)
	   AlexLastAcc k c' (p',inp') len ->
 	     k p c inp len (\scs -> alex_gscan stop p' c' inp' scs) 
		(IBOX(sc),state)

#endif

-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn lc c len (ILIT(-1)) last_acc = return last_acc
alex_scan_tkn lc c len s last_acc = do
  new_acc <- check_accs (alex_accept `unsafeAt` IBOX(s))
  c <- alexGetChar
  case c of
    Nothing -> return new_acc	-- end of input
    Just c' ->
#ifdef ALEX_DEBUG
	trace ("State: " ++ show IBOX(s) ++ ", char: " ++ show c') $
#endif
	alex_scan_tkn lc c' 
		PLUS(len,ILIT(1)) s' new_acc
      	where
		base   = alexIndexShortOffAddr alex_base s
		IBOX(ord_c) = ord c'
		offset = PLUS(base,ord_c)
		check  = alexIndexShortOffAddr alex_check offset

		s' = 
		     if GTE(offset,ILIT(0)) && EQ(check,ord_c)
			then alexIndexShortOffAddr alex_table offset
			else alexIndexShortOffAddr alex_deflt s
   where
	check_accs [] = return last_acc
	check_accs (AlexAcc _ a lctx rctx : rest) = 
	  case lctx of
	    Nothing  -> check_rctx
	    Just arr | arr!lc    -> check_rctx
		     | otherwise -> fail
	  where
	    fail = check_accs rest

	    ok = do inp <- alexGetInput
		    return (AlexLastAcc a c inp IBOX(len))

	    check_rctx = 
		case rctx of
		   Nothing -> ok
		   Just IBOX(sn) -> do
		      inp <- alexGetInput
		      acc <- alex_scan_tkn c c ILIT(0) sn AlexNone
		      alexSetInput inp
		      case acc of
			AlexNone      -> fail
			AlexLastAcc{} -> ok
			-- TODO: there's no need to find the longest
			-- match when checking the right context, just
			-- the first match will do.

data AlexLastAcc a = AlexNone | AlexLastAcc a Char AlexInput Int

data AlexAcc a = AlexAcc Int a (Maybe (Array Char Bool)) (Maybe Int)


