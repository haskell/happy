{------------------------------------------------------------------------------
				      NFA GENERATOR

The `scanner2nfa' takes a `Scanner' (see the `RExp' module) and generates its
equivelent nondeterministic finite automaton.  NFAs are turned into DFAs in the
DFA module.

See the chapter on `Finite Automata and Lexical Analysis' in the dragon book
for an excellent overview of the algorithms in this module.

Chris Dornan, Aug-95, 10-Jul-96, 29-Sep-97
------------------------------------------------------------------------------}

module NFA where

import Array
import DFS
import Alex
import AbsSyn
import CharSet


-- Each state of a nondeterministic automaton contains a list of `Accept'
-- values, a list of epsilon transitions (an epsilon transition represents a
-- transition to another state that can be made without reading a character)
-- and a list of transitions qualified with a character predicate (the
-- transition can only be made to the given state on input of a character
-- permitted by the predicate).  Although a list of `Accept' values is provided
-- for, in actual fact each state will have zero or one of them (the `Maybe'
-- type is not used because the flexibility offered by the list representation
-- is useful).

type NFA = Array SNum NState

data NState = NSt [Accept Code] [SNum] [(CharSet,SNum)]

nst_accs:: NState -> [Accept Code]
nst_accs (NSt accs _ _) = accs

nst_cl:: NState -> [SNum]
nst_cl (NSt _ es _) = es

nst_outs:: NState -> [(CharSet,SNum)]
nst_outs (NSt _ _ outs) = outs


{- 			     From the Scan Module

-- The `Accept' structure contains the priority of the token being accepted
-- (lower numbers => higher priorities), the name of the token, a place holder
-- that can be used for storing the `action' function, a list of start codes
-- (listing the start codes that the scanner must be in for the token to be
-- accepted; empty => no restriction), the leading and trailing context (both
-- `Nothing' if there is none).
--  
-- The leading context consists simply of a character predicate that will
-- return true if the last character read is acceptable.  The trailing context
-- consists of an alternative starting state within the DFA; if this `sub-dfa'
-- turns up any accepting state when applied to the residual input then the
-- trailing context is acceptable.

data Accept a = Acc Int String a [StartCode] (Maybe CharSet) (Maybe SNum)
-}


-- `scanner2nfa' takes a scanner (see the RExp module) and converts it to an
-- NFA.  It works by converting each of the regular expressions to a partial
-- NFA.  Partial NFAs differ from NFAs in being composable (see below).  These
-- partial NFAs are sequentially composed with accepting partial NFAs that
-- record all the details of the tokens being accepted, including its priority
-- (token definitions appearing earlier in the scanner take priority over later
-- ones), name and context specifications; see the `acc_nfa' below for more
-- details of accepting NFAs.  The list of partial NFAs are then composed with
-- the `bar_nfa' operator and converted to an NFA.

scanner2nfa:: Scanner -> NFA
scanner2nfa Scanner{scannerTokens = toks}
   = mk_nfa (foldr bar_nfa eps_nfa pnfas)
	where
	pnfas = [mk_pnfa n re| (re, n)<-zip toks [0..]]

	mk_pnfa n (RECtx scs lctx re rctx code) =
		   		rexp2pnfa re `seq_nfa` acc_nfa rctx' mk_acc
		where
		mk_acc rctx'' = Acc n code (map snd scs) lctx' rctx''

		rctx' =	case rctx of
			  Nothing -> Nothing
			  Just re -> Just(rexp2pnfa re)

		lctx' = case lctx of
			  Nothing -> Nothing
			  Just st -> Just st



{------------------------------------------------------------------------------
				 Partial NFAs
------------------------------------------------------------------------------}



rexp2pnfa:: RExp -> PartNFA
rexp2pnfa Eps = eps_nfa
rexp2pnfa (Ch p) = ch_nfa p
rexp2pnfa (re :%% re') = rexp2pnfa re `seq_nfa` rexp2pnfa re'
rexp2pnfa (re :| re') = rexp2pnfa re `bar_nfa` rexp2pnfa re'
rexp2pnfa (Star re) = star_nfa (rexp2pnfa re)
rexp2pnfa (Plus re) = plus_nfa (rexp2pnfa re)
rexp2pnfa (Ques re) = ques_nfa (rexp2pnfa re)


-- `PartNFA' is a composable NFA.  It consists of a pair containing the size of
-- the automaton (i.e., the number of states used) and a function for
-- constructing the automaton.  The function takes the base of the automaton
-- (so that it will use the state numbers from the base up to base+size-1), the
-- exit state for the automaton and the remaining states to be appended to the
-- automaton, in reverse order; the function then returns all the states in the
-- automaton in reverse order.  By convention, every automaton should entered
-- throgh its base state.

type PartNFA = (Int, SNum->SNum->[NState]->[NState])
--	in mk_nfa, eps_nfa, acc_nfa, ch_nfa,
--	   seq_nfa, bar_nfa, star_nfa, plus_nfa, ques_nfa


-- `mk_nfa' converts a partial NFA to an NFA.  Apart from converting to an
-- array format it must close all the epsilon transitions, so that if there is
-- an epsilon transition from s1 to s2 and s2 to s3 then s3 must be listed in
-- the epsilon transitions of s1.  This is done by `e_close' which uses the
-- DFS function `t_close' for computing the transitive closure of a graph.

mk_nfa:: PartNFA -> NFA
mk_nfa (sz,f) = e_close ar
	where
	ar = listArray (0,sz) (reverse(err:f 0 sz []))
	err = NSt [] [] []

e_close:: Array Int NState -> NFA
e_close ar = listArray bds
		[NSt accs (out gr v) outs|(v,NSt accs _ outs)<-assocs ar]
	where
	gr = t_close (hi+1,\v->nst_cl (ar!v))
	bds@(_,hi) = bounds ar


-- `acc_nfa' constructs a partial NFA that accepts a token.  It is essentially
-- an epsilon NFA with an accepting state.  It would be quite straightforward
-- except for the need to deal with trailing context.  Tokens with trailing
-- context are encoded by converting the trailing-context regular expression to
-- a partial NFA with a dummy accept state, placing it out of the way on the
-- end of the partial NFA under construction and making a note of its entry
-- state in the main accepting state.  Thus, if there is any trailing context,
-- the `acc_nfa' function will receive a partial NFA in its first argument and
-- it will pass the state number of the corresponding entry point to the
-- function in its second argument to get the accept value; if there is no
-- trailing context then it passes `Nothing' to the Accept-constructing
-- function.

acc_nfa:: Maybe PartNFA -> (Maybe SNum->Accept Code) -> PartNFA
acc_nfa Nothing mk_act = (1,\bse ex sts->NSt [mk_act Nothing] [ex] []:sts)
acc_nfa (Just pnfa) mk_acc = (1+sz,g)
	where
	g bse ex sts = f (bse+1) (-1) (NSt [mk_acc(Just(bse+1))] [ex] []:sts)

	(sz,f) = pnfa `seq_nfa` acc_pnfa

	acc_pnfa = (1,\bse ex sts->NSt [rctxt_accept] [] [] : sts)

rctxt_accept :: Accept Code
rctxt_accept = Acc 0 "trailing context accept" [] Nothing Nothing

-- The following functions compose NFAs according to the `RExp' operators.  The
-- construction are more compact than those given in the dragon book.

eps_nfa:: PartNFA
eps_nfa = (1,\bse ex sts->NSt [] [ex] []:sts)

ch_nfa:: CharSet -> PartNFA
ch_nfa st = (1,\bse ex sts->NSt [] [] [(st,ex)]:sts)

seq_nfa:: PartNFA -> PartNFA -> PartNFA
seq_nfa (sz,f) (sz',g) = (sz+sz',h)
	where
	h bse ex sts = g (bse+sz) ex (f bse (bse+sz) sts)

bar_nfa:: PartNFA -> PartNFA -> PartNFA
bar_nfa (sz,f) (sz',g) = (sz+sz'+1,h)
	where
	h bse ex sts = g (bse+sz+1) ex (f (bse+1) ex (alt_st:sts))
		where
		alt_st = NSt [] [bse+1,bse+sz+1] []

star_nfa:: PartNFA -> PartNFA
star_nfa (sz,f) = (sz+1,h)
	where
	h bse ex sts = f (bse+1) bse (NSt [] [ex,bse+1] []:sts)

plus_nfa:: PartNFA -> PartNFA
plus_nfa (sz,f) = (sz+1,h)
	where
	h bse ex sts = NSt [] [ex,bse] []:f bse (bse+sz) sts

ques_nfa:: PartNFA -> PartNFA
ques_nfa (sz,f) = (sz+1,h)
	where
	h bse ex sts = f (bse+1) ex (NSt [] [ex,bse+1] []:sts)
