%-----------------------------------------------------------------------------
Main.lhs
(c) Andy Gill, Simon Marlow 1993
%-----------------------------------------------------------------------------

> module Main (main) where

> import GenUtils
> import OurIO
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

> main = runOurIO (

Read and parse the CLI arguments.

>       runStrListCont getArgs				>>= \ args ->
>       case parseArgs (constArgs ++ args) argFns of
>         	(cli,[fl_name],[]) -> runParserGen cli fl_name
>               (cli,[],[]) | DumpVerbose `elem` cli -> copyright
>		(_,_,errors) -> error (unlines errors ++ "\n" ++ syntax))

>  where 	
>    runParserGen cli fl_name =


>       optIO (elem DumpVerbose cli) copyright		>>


Open the file.

>       runStrCont (readFile fl_name)	     		>>= \ fl ->

>       let (file,name) = possDelit (reverse fl_name) fl 
>       in
>       optPrint cli DumpInFile (printOurIO file) 
>							>>


Perform Lexical Analysis.

>       let toks = ourLexer file
>       in

#ifdef DEBUG

>       optPrint cli DumpLex (printOurIO (show toks))             >>

#endif


Parse, using bootstrapping parser.

>       let abssyn@(AbsSyn hd _ _ tl) = ourParser toks
>       in

#ifdef DEBUG

>       optPrint cli DumpParse (printOurIO (show abssyn))         >>

#endif

Mangle the syntax into something useful.

>	case scc "Mangler" (mangler abssyn) of {
>		Failed s -> 
>			runSuccCont (appendChan stderr (unlines s)) >>
>			returnOurIO ();
>		Succeeded g -> 
>	

>	let gram@(Grammar gram_info dir term nonterm tys env 
>	        eof@(Terminal eof_no)) = g
>       in



#ifdef DEBUG

>       optPrint cli DumpMangle (printOurIO (show gram)) >>

#endif


>       let first  	= scc "First" (mkFirst gram_info)
>           items  	= scc "Items" (genLR0items gram_info)
>	    lainfo@(spont,prop) = scc "Prop" (propLookaheads gram_info items first)
>	    la 		= scc "Calc" (calcLookaheads ((0,(0,0),eof):spont) prop)
>	    items'	= scc "Merge" (mergeLookaheadInfo la items)
>           goto   	= scc "Goto" (genGotoTable gram_info items)
>           action 	= scc "Action" (genActionTable gram_info first items')
>	    (conflictArray,(sr,rr))   = scc "Conflict" (countConflicts action)
>       in

#ifdef DEBUG

>       optPrint cli DumpLR0 (printOurIO (show items))		>>
>       optPrint cli DumpAction (printOurIO (show action))        	>>
>       optPrint cli DumpGoto (printOurIO (show goto))            	>>
>       optPrint cli DumpLA (printOurIO (show lainfo))		>>
>       optPrint cli DumpLA (printOurIO (show la))			>>

#endif


>	(if sr /= 0
>		then runSuccCont (appendChan stdout
>		 ("shift/reduce conflicts:  " ++ show sr ++ "\n"))
>		else returnOurIO ())			>>

>	(if rr /= 0
>		then runSuccCont (appendChan stdout
>		 ("reduce/reduce conflicts: " ++ show rr ++ "\n"))
>		else returnOurIO ())			>>

Print out the file.

>	let info = genInfoFile
>			env
>			(map fst items)
>			gram_info
>			action
>			goto
>			[ (a,b) | (Terminal a,b) <- getTokenSpec dir]
>			conflictArray
>			fl_name
>	    info_filename = getInfoFileName name cli
>	in
>	

>	(case info_filename of
>		Just s  -> runSuccCont (writeFile s info)
>		Nothing -> returnOurIO ())			>>

Now, lets get on with generating the parser.  Firstly, find out what kind
of code we should generate, and where it should go:

>	let target   = getTarget cli
>	    template = template_file (getTemplate template_dir cli) target
>	    outfilename = getOutputFileName fl_name cli in

Read in the template file for this target:

>       runStrCont (readFile template)			>>= \ templ ->

and generate the code.

>       let outfile = produceParser 
>                       gram_info
>                       action
>                       goto
>                       (getNewline dir)
>                       [ (a,b) | (Terminal a,b) <- getTokenSpec dir]
>                       (getTokenType dir)
>			tys
>			(getParserName dir)
>                       hd
>                       tl
>			target
>       in

>       runSuccCont (writeFile outfilename (outfile ++ templ))

Successfully Finished.

>	}

%-----------------------------------------------------------------------------

> optIO :: Bool -> OurIO () -> OurIO ()
> optIO fg io = if fg then io >> returnOurIO () else returnOurIO ()

> optPrint cli pass io = 
>       optIO (elem pass cli) 
>               (appendChan stdout "\n---------------------\n" exit . io)

> optDump cli pass io =
> 	optIO (elem pass cli) io

> printIO str = appendChan stdout (show str) exit

%------------------------------------------------------------------------------

The constArgs list is a hack to pass command line arguments to the
program when developing in gofer.

#ifdef __GOFER__

> constArgs = words "tests/AndysTest.ly"

#else

> constArgs = []

#endif

%------------------------------------------------------------------------------

> possDelit :: String -> String -> (String,String)
> possDelit ('y':'l':'.':nm) fl = (deLitify fl,reverse nm)
> possDelit ('y':'.':nm) fl     = (fl,reverse nm)
> possDelit _            fl     = error "Filename suffix unknown"

This was a program hot-spot, but not any more.

> deLitify :: String -> String
> deLitify = deLit 
>  where 
>       deLit ('>':' ':r)  = deLit1 r
>       deLit ('>':'\t':r)  = "      " ++ deLit1 r
>       deLit ('>':r)  = error "Error when de-litify-ing"
>       deLit ('\n':r) = '\n' : deLit r
>       deLit r        = deLit2 r
>       deLit1 ('\n':r) = '\n' : deLit r
>       deLit1 (c:r)    = c : deLit1 r
>       deLit1 []       = []
>       deLit2 ('\n':r) = '\n' : deLit r
>       deLit2 (c:r)    = deLit2 r
>       deLit2 []       = []

%------------------------------------------------------------------------------

The command line arguments.

> argFns "-infile"      = flag DumpInFile
> argFns "-lex"         = flag DumpLex
> argFns "-parse"       = flag DumpParse
> argFns "-mangle"      = flag DumpMangle
> argFns "-lr0"         = flag DumpLR0
> argFns "-action"      = flag DumpAction
> argFns "-goto"        = flag DumpGoto
> argFns "i"            = flagWithOptArg OptInfoFile
> argFns "-info"        = flagWithOptArg OptInfoFile
> argFns "-template"    = flagWithArg OptTemplate
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
>               | DumpLex 
>               | DumpParse 
>               | DumpMangle
>               | DumpVerbose
>               | DumpLR0
>               | DumpAction
>               | DumpGoto
>               | DumpOutFile
>               | DumpNever
>		| DumpLA
>		
>		| OptInfoFile (Maybe String)
>		| OptTemplate String
>
>		| OptGhcTarget
>		| OptArrayTarget
>		
>		| OptOutputFile String
>  deriving (Eq{-,Text-})

#ifdef __GOFER__

> instance Text CLIFlags where { showsPrec = primPrint } 
> instance Eq CLIFlags where { (==) = primGenericEq } 

#endif

%-----------------------------------------------------------------------------
How would we like our code to be generated?

> getTarget :: [CLIFlags] -> Target
> getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
> 			[t] -> t
>			[]  -> TargetHaskell
>			_   -> error "multiple target options"

> optToTarget OptGhcTarget 	= Just TargetGhc
> optToTarget OptArrayTarget 	= Just TargetArrayBased
> optToTarget _			= Nothing

> template_file temp_dir target 
>   = temp_dir ++
>	  (case target of
>		TargetHaskell 	 -> "/HappyTemplate"
>		TargetGhc	 -> "/HappyTemplate-ghc"
>		TargetArrayBased -> "/HappyTemplate-arrays")


#ifdef __GOFER__

> template_dir = "templates/" {-

#endif

> template_dir = HAPPYLIB

#ifdef __GOFER__

> -}

#endif

%------------------------------------------------------------------------------
Where would we like our parser and info file ?

> getOutputFileName ip_file cli
> 	= case [ s | (OptOutputFile s) <- cli ] of
>		[]  -> base ++ ".hs"
>			where (base,ext) = break (== '.') ip_file
>		[f] -> f
>		_   -> error "multiple -o options"

> getInfoFileName base cli
> 	= case [ s | (OptInfoFile s) <- cli ] of
>		[]	   -> Nothing
>		[Nothing]  -> Just (base ++ ".info")
>		[Just f]   -> Just f
>		_   -> error "multiple -i options"

> getTemplate def cli
> 	= case [ s | (OptTemplate s) <- cli ] of
>		[]	   -> def
>		[f]        -> f
>		_          -> error "multiple templates specified"


%------------------------------------------------------------------------------

> copyright :: OurIO ()
> copyright = printOurIO (unlines  [
>  "Happy Version " ++ version ++ " Copyright (c) Andy Gill, Simon Marlow 1993-1995","",
>  "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
>  "This program is free software; you can redistribute it and/or modify",
>  "it under the terms of Version 2 of the GNU General Public License",
>  "as published by the Free Software Foundation.","",
>  "Look at the file README distributed with the source for details.",""])


> syntax = unlines [
>   "syntax: happy [-v] [--outfile] [--info [file]]",
>   "              [-g | --ghc] [-a | --array] [-o [file]] file"
>	]

%-----------------------------------------------------------------------------

#ifndef __GLASGOW_HASKELL__

> scc _ = id

#endif


