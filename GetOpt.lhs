%-----------------------------------------------------------------------------
GetOpt.lhs
(c) Andy Gill, Simon Marlow 1993
%-----------------------------------------------------------------------------

Functions for parsing command line arguments.

> module GetOpt (parseArgs, flag, flagWithArg, flagWithOptArg) where

> import GenUtils

> parseArgs 
> 	:: [String]					-- The command line
>	-> (String -> [String] -> Maybe ([String],a))	-- arg recogniser
>	-> ( [a],					-- flags found
>	     [String],					-- arguments left over
>	     [String] )					-- error messages	

> parseArgs ("-":args) recog = parseArgs args recog
> parseArgs args recog = parseArgs' args [] [] []
>	where
> 	parseArgs' [] flags notflags errors = (flags,notflags,errors)
> 	parseArgs' ((c:arg):args) flags notflags errors
> 		= case c of
>	   	    '-' -> case recog arg args of
>			Just (rest,flag) 
>				-> parseArgs' rest (flag:flags) notflags errors
>			Nothing   
>				-> parseArgs' args flags notflags
>				(("unknown flag: -" ++ arg):errors)
>	   	    _   -> parseArgs' args flags ((c:arg):notflags) errors

%-----------------------------------------------------------------------------
Functions for building up a flag recogniser function.  The flag
recogniser usually has the following form:

f a = case a of
  "a" -> flag MinusA
  "b" -> flagWithArg MinusB
  "c" -> flagWithOptArg MinusC

(the leading minus is stripped from the first argument, you can have
POSIX style flags (eg. --flag) by using the pattern "-flag")

You'll need the corresponding flag datatype, eg.:

data Flag 
	= MinusA
	| MinusB String
	| MinusC (Maybe String)

> flag 		 :: a 			-> [String] -> Maybe ([String],a)
> flagWithArg 	 :: (String -> a) 	-> [String] -> Maybe ([String],a)
> flagWithOptArg :: (Maybe String -> a) -> [String] -> Maybe ([String],a)

> flag          flagName args 		= Just (args,flagName)
> flagWithArg	flagName (arg:args) 	= Just (args,flagName arg)
> flagWithOptArg flagName args
> 	= let noarg = Just (args, flagName Nothing) in
>	  case args of
>		(('-':arg):_) -> noarg
>		(arg:args)    -> Just (args, flagName (Just arg))
>		[]            -> noarg

