> {
> module PgnParser (pgnMoveParser,pgnParser) where

> import GenUtils
> import OrdFM
> import Board
> import PgnTypes
> }

> %name pgnMoveParser
> %tokentype { Token }
> %token 
>	str		{ StringToken $$ }
>	result		{ ResultToken $$ }
>	nag		{ NAGToken $$ }
>	tag		{ TagToken $$ }
>	comment		{ CommentToken $$ }
>	']'		{ RightSBToken }
>	'('		{ LeftRBToken }
>	')'		{ RightRBToken }
>	'<'		{ LeftABToken }
>	'>'		{ RightABToken }
>	num		{ IntToken $$ }
>	'.'		{ PeriodToken }
> 	move		{ PlyToken $$ }
> %newline            	{ NewlineToken }

> %%

You either parse a set of PGN games,
or just a set of moves.

> moves :: { AbsMove }
> moves : opt_mv_num line_no move nags opt_comment analmoves opt_comment 
>	  more_moves
>			{ AbsMove $1 $2 $3 $4 ($5++$7) $6 Nothing $8 }
>       | opt_mv_num line_no move nags opt_comment more_moves
>			{ AbsMove $1 $2 $3 $4 $5 [] Nothing $6 }
>       | opt_mv_num line_no move '<' raw_moves '>' more_moves
>			{ AbsMove $1 $2 $3 [] [] [] (Just $5) $7 }

> more_moves :: { AbsMove }
> more_moves 
>	: moves			{ $1 }
>	| result		{ AbsResult $1 }
>	| 			{ AbsEnd }

> nags :: { [Int] }
> nags	: nag nags		{ $1 : $2 }
>	|			{ [] }

> opt_mv_num :: { Maybe MoveNumber }
> opt_mv_num 
>	: num '.' '.' '.'	{ Just (MoveNumber $1 Black) }
>	| num '.'		{ Just (MoveNumber $1 White) }
>	|			{ Nothing }

> mv_num :: { MoveNumber }
> mv_num 
>	: num '.' '.' '.'	{ (MoveNumber $1 Black) }
>	| num '.'		{ (MoveNumber $1 White) }

> opt_comment :: { [String] }
> opt_comment 
>	: comment		{ $1 }
>	|			{ [] } 

> analmoves :: { [AbsMove] }
> analmoves
>	: '(' moves ')'	analmoves	{ $2 : $4 }
>	| '(' moves ')'			{ [$2] }

> line_no :: { LineNo }
> line_no 
>	: 				{ $# }

> raw_moves :: { [AbsPly] }
> raw_moves 
>	: move raw_moves		{ $1 : $2 }
>	| 				{ [] }


			

> {

> pgnParser = pgnGameMap pgnMoveParser

> happyError :: Int -> [Token] -> a
> happyError i xs = 
> 	error ("Parse error in line " ++ show i ++ "\n"
>		++ show (take 10 xs))

> }

