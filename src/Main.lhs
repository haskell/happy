-----------------------------------------------------------------------------
$Id: Main.lhs,v 1.13 1997/10/03 16:29:44 andre Exp $

The main driver.

(c) 1993-1997 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Main (main) where

> import ParseMonad
> import GenUtils
> import Lexer
> import AbsSyn
> import Grammar
> import Parser
> import First
> import LALR
> import Version
> import ProduceCode (produceParser)
> import Info (genInfoFile)
> import Target (Target(..))
> import GetOpt
> import Set

#if __HASKELL1__ >= 3 && ( !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ >= 200 )

> import System
> import Char
> import IO
> import Array( Array, assocs, elems, (!) )
> import List( nub )

#define ASSOC(a,b) (a , b)

#else

#define ASSOC(a,b) (a := b)

> import
>	LibSystem

#endif

#ifdef __GLASGOW_HASKELL__
#define sCC _scc_
#endif

> main = 

Read and parse the CLI arguments.

>       getArgs				>>= \ args ->
>	main2 args

> main2 :: [String] -> IO ()
> main2 args = 

Read and parse the CLI arguments.

>       case parseArgs (constArgs ++ args) argFns of
>         	(cli,[fl_name],[]) -> runParserGen cli fl_name
>               (cli,[],[]) | DumpVerbose `elem` cli -> copyright
>		(_,_,errors) -> die (unlines errors ++ "\n" ++ syntax)

>  where 	
>    runParserGen cli fl_name =

Print out the copyright message if we are in verbose mode.

>       optIO (elem DumpVerbose cli) copyright		>>

Open the file.

>       readFile fl_name		     		>>= \ fl ->
>	possDelit (reverse fl_name) fl			>>= \ (file,name) ->
>       optPrint cli DumpInFile (putStr file) 		>>

Parse, using bootstrapping parser.

>	case ourParser file 1 of {
>		FailP err -> die (fl_name ++ ':' : err);
>		OkP abssyn@(AbsSyn hd _ _ tl) -> 

#ifdef DEBUG

>       optPrint cli DumpParse (putStr (show abssyn))         >>

#endif

Mangle the syntax into something useful.

>	case sCC "Mangler" (mangler abssyn) of {
>		Failed s -> die (unlines s ++ "\n");
>		Succeeded g -> 

>	let gram@(Grammar gram_info dir term nonterm tys env eof) = g
> 	    term_dir = [ (a,b) | (a,b) <- getTokenSpec dir, a >= first_term]
>	    first_term = getFirstTerm gram_info
>       in


#ifdef DEBUG

>       optPrint cli DumpMangle (putStr (show gram)) >>

#endif


>       let first  	= sCC "First" (mkFirst gram_info)
>	    closures    = sCC "Closures" (precalcClosure0 gram_info)
>           sets  	= sCC "LR0 Sets" (genLR0items gram_info closures)
>	    lainfo@(spont,prop) = sCC "Prop" (propLookaheads gram_info sets first)
>	    la 		= sCC "Calc" (calcLookaheads (length sets)
>					((0,(0,0),[eof]):spont) prop)
>	    items2	= sCC "Merge" (mergeLookaheadInfo la sets)
>           goto   	= sCC "Goto" (genGotoTable gram_info sets)
>           action 	= sCC "Action" (genActionTable gram_info first items2)
>	    (conflictArray,(sr,rr))   = sCC "Conflict" (countConflicts action)
>       in

#ifdef DEBUG

>       optPrint cli DumpLR0 (putStr (show sets))		>>
>       optPrint cli DumpAction (putStr (show action))      	>>
>       optPrint cli DumpGoto (putStr (show goto))          	>>
>       optPrint cli DumpLA (putStr (show lainfo))		>>
>       optPrint cli DumpLA (putStr (show la))			>>

#endif

Report any unused rules and terminals

>	let (unused_rules, unused_terminals) = 
>		find_redundancies gram_info env action
>	in
>	optIO (not (null unused_rules))
>	   (putStr ("unused rules: " ++ show (length unused_rules) ++ "\n")) >>
>	optIO (not (null unused_terminals))
>	   (putStr ("unused terminals: " ++ show (length unused_terminals) ++
>		"\n")) >>

Report any conflicts in the grammar.

>	(if sr /= 0
>		then putStr ("shift/reduce conflicts:  " ++ show sr ++ "\n")
>		else return ())			>>

>	(if rr /= 0
>		then putStr ("reduce/reduce conflicts: " ++ show rr ++ "\n")
>		else return ())			>>

Print out the info file.

>	getInfoFileName name cli		>>= \info_filename ->
>	let info = genInfoFile
>			env
>			(map fst sets)
>			gram_info
>			action
>			goto
>			term_dir
>			conflictArray
>			fl_name
>			unused_rules
>			unused_terminals
>	in

>	(case info_filename of
>		Just s  -> writeFile s info
>		Nothing -> return ())			>>

Now, lets get on with generating the parser.  Firstly, find out what kind
of code we should generate, and where it should go:

>	getTarget cli					>>= \target ->
>	getOutputFileName fl_name cli			>>= \outfilename ->
>	getTemplate template_dir cli			>>= \template' ->
>	let template = template_file template' target (Opt1_2 `elem` cli) in

Read in the template file for this target:

>       readFile template				>>= \ templ ->

and generate the code.

>	getMagicName cli				>>= \ magic_name ->
>       let outfile = produceParser 
>                       gram_info
>                       action
>                       goto
>                       (getLexer dir)
>                       term_dir
>                       (getTokenType dir)
>			tys
>			(getParserName dir)
>			(getMonad dir)
>                       hd
>                       tl
>			target
>			opt1_2
>	    opt1_2 = Opt1_2 `elem` cli
>	    magic_filter = 
>	      case magic_name of
>		Nothing -> id
>		Just name ->
>		  let
>		      small_name = name
>		      big_name = toUpper (head name) : tail name
>		      filter_output ('h':'a':'p':'p':'y':rest) =
>			small_name ++ filter_output rest
>		      filter_output ('H':'a':'p':'p':'y':rest) =
>			big_name ++ filter_output rest
>		      filter_output (c:cs) = c : filter_output cs
>		      filter_output [] = []
>		  in 
>		     filter_output 
>       in

>       writeFile outfilename (magic_filter (outfile ++ templ))

Successfully Finished.

>	}}

-----------------------------------------------------------------------------

> die :: String -> IO a
> die s = hPutStr stderr s >> exitWith (ExitFailure 1)

> dieHappy :: String -> IO a
> dieHappy s = getProgName >>= \prog -> die (prog ++ ": " ++ s)

> optIO :: Bool -> IO a -> IO a
> optIO fg io = if fg then io  else return (error "optIO")

> optPrint cli pass io = 
>       optIO (elem pass cli) (putStr "\n---------------------\n" >> io)

> optDump cli pass io =
> 	optIO (elem pass cli) io

> constArgs = []

-----------------------------------------------------------------------------
Find unused rules and tokens

> find_redundancies 
>	:: GrammarInfo 
>	-> Array Name String
>	-> ActionTable
>	-> ([Int], [String])
>
> find_redundancies g env action_table = 
>	(unused_rules, map (env !) unused_terminals)
>    where
>	actions		 = concat (map assocs (elems action_table))
>	used_rules       = 0 : nub [ r | ASSOC(_,LR'Reduce{-'-} r) <- actions ]
>	used_tokens      = errorTok : eof : 
>			       nub [ t | ASSOC(t,LR'Shift{-'-} _ ) <- actions ]
>	terminals        = getTerminals g
>	non_terminals    = getNonTerminals g
>	eof		 = getEOF g
>	n_prods		 = length (getProds g)
>	unused_terminals = filter (`notElem` used_tokens) terminals
>	unused_rules     = filter (`notElem` used_rules ) [0..n_prods-1]

------------------------------------------------------------------------------

> possDelit :: String -> String -> IO (String,String)
> possDelit ('y':'l':'.':nm) fl = return (deLitify fl,reverse nm)
> possDelit ('y':'.':nm) fl     = return (fl,reverse nm)
> possDelit f            fl     = 
>	dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")

This was a program hot-spot, but not any more.

> deLitify :: String -> String
> deLitify = deLit 
>  where 
>       deLit ('>':' ':r)  = deLit1 r
>       deLit ('>':'\t':r)  = '\t' : deLit1 r
>       deLit ('>':r)  = error "Error when de-litify-ing"
>       deLit ('\n':r) = '\n' : deLit r
>       deLit r        = deLit2 r
>       deLit1 ('\n':r) = '\n' : deLit r
>       deLit1 (c:r)    = c : deLit1 r
>       deLit1 []       = []
>       deLit2 ('\n':r) = '\n' : deLit r
>       deLit2 (c:r)    = deLit2 r
>       deLit2 []       = []

------------------------------------------------------------------------------

The command line arguments.

> argFns "-infile"      = flag DumpInFile
> argFns "-parse"       = flag DumpParse
> argFns "-mangle"      = flag DumpMangle
> argFns "-lr0"         = flag DumpLR0
> argFns "-action"      = flag DumpAction
> argFns "-goto"        = flag DumpGoto
> argFns "1.2"          = flag Opt1_2
> argFns "i"            = flagWithOptArg OptInfoFile
> argFns "-info"        = flagWithOptArg OptInfoFile
> argFns "-template"    = flagWithArg OptTemplate
> argFns "-magic-name"  = flagWithArg OptMagicName
> argFns "v"            = flag DumpVerbose
> argFns "-verbose"     = flag DumpVerbose
> argFns "-lookaheads"  = flag DumpLA
> argFns "g"		= flag OptGhcTarget
> argFns "-ghc"		= flag OptGhcTarget
> argFns "a"		= flag OptArrayTarget
> argFns "-array"	= flag OptArrayTarget
> argFns "o"		= flagWithArg OptOutputFile
> argFns "-outfile"     = flagWithArg OptOutputFile
> argFns _              = const Nothing

> data CLIFlags = DumpInFile
>               | DumpParse 
>               | DumpMangle
>               | DumpVerbose
>               | DumpLR0
>               | DumpAction
>               | DumpGoto
>		| DumpLA
>		
>		| OptInfoFile (Maybe String)
>		| OptTemplate String
>		| OptMagicName String
>		| Opt1_2
>
>		| OptGhcTarget
>		| OptArrayTarget
>		
>		| OptOutputFile String
>  deriving (Eq{-,Text-})

-----------------------------------------------------------------------------
How would we like our code to be generated?

> optToTarget OptGhcTarget 	= Just TargetGhc
> optToTarget OptArrayTarget 	= Just TargetArrayBased
> optToTarget _			= Nothing

> template_file temp_dir target haskell_1_2
>   = temp_dir ++ base ++
>     if haskell_1_2 then "-1_2" else ""
>  where  
>	base = case target of
>		 TargetHaskell 	  -> "/HappyTemplate"
>		 TargetGhc	  -> "/HappyTemplate-ghc"
>		 TargetArrayBased -> "/HappyTemplate-arrays"

------------------------------------------------------------------------------
Extract various command-line options.

> getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
> 			[t] -> return t
>			[]  -> return TargetHaskell
>			_   -> dieHappy "multiple target options\n"

> getOutputFileName ip_file cli
> 	= case [ s | (OptOutputFile s) <- cli ] of
>		[]  -> return (base ++ ".hs")
>			where (base,ext) = break (== '.') ip_file
>		[f] -> return f
>		_   -> dieHappy "multiple -o options\n"

> getInfoFileName base cli
> 	= case [ s | (OptInfoFile s) <- cli ] of
>		[]	   -> return Nothing
>		[Nothing]  -> return (Just (base ++ ".info"))
>		[Just f]   -> return (Just f)
>		_   -> dieHappy "multiple -i options\n"

> getTemplate def cli
> 	= case [ s | (OptTemplate s) <- cli ] of
>		[]	   -> return def
>		[f]        -> return f
>		_          -> dieHappy "multiple templates specified\n"

> getMagicName cli
> 	= case [ s | (OptMagicName s) <- cli ] of
>		[]	   -> return Nothing
>		[f]        -> return (Just (map toLower f))
>		_          -> dieHappy "multiple --magic-name options\n"


------------------------------------------------------------------------------

> copyright :: IO ()
> copyright = putStr (unlines  [
>  "Happy Version " ++ version ++ " Copyright (c) Andy Gill, Simon Marlow 1993-1997","",
>  "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
>  "This program is free software; you can redistribute it and/or modify",
>  "it under the terms of Version 2 of the GNU General Public License",
>  "as published by the Free Software Foundation.","",
>  "Look at the file README distributed with the source for details.\n"])


> syntax = unlines [
>   "syntax: happy [-v] [-o | --outfile <file>] [--info [<file>]]",
>   "		   [-1.2] [--template <dir>]",
>   "              [-g | --ghc] [-a | --array] <file>\n" ]

> template_dir = "/usr/local/lib/happy"

-----------------------------------------------------------------------------

#ifndef __GLASGOW_HASKELL__

> sCC _ = id

#endif


