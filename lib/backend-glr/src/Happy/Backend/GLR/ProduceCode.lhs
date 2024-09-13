Module for producing GLR (Tomita) parsing code.
This module is designed as an extension to the Haskell parser generator Happy.

(c) University of Durham, Ben Medlock 2001
        -- initial code, for structure parsing
(c) University of Durham, Paul Callaghan 2004
        -- extension to semantic rules, and various optimisations
%-----------------------------------------------------------------------------

> module Happy.Backend.GLR.ProduceCode
>                       ( produceGLRParser
>                       , baseTemplate
>                       , libTemplate
>                       , DecodeOption(..)
>                       , FilterOption(..)
>                       , GhcExts(..)
>                       , Options
>                       ) where

> import Paths_happy_lib ( version )
> import Happy.Grammar
> import Happy.Tabular.LALR
> import Data.Array ( Array, (!), array, assocs )
> import Data.Char ( isSpace, isAlphaNum )
> import Data.List ( nub, (\\), sort, find, tails )
> import Data.Version ( showVersion )

%-----------------------------------------------------------------------------
File and Function Names

> baseTemplate, libTemplate :: String -> String
> baseTemplate td = td ++ "/GLR_Base.hs"          -- NB Happy uses / too
> libTemplate  td = td ++ "/GLR_Lib.hs"           -- Windows accepts this?

---
prefix for production names, to avoid name clashes

> prefix :: String
> prefix = "G_"

%-----------------------------------------------------------------------------
This type represents choice of decoding style for the result

> data DecodeOption
>  = TreeDecode
>  | LabelDecode

---
This type represents whether filtering done or not

> data FilterOption
>  = NoFiltering
>  | UseFiltering

---
This type represents whether GHC extensions are used or not
 - extra values are imports and ghc options reqd

> data GhcExts
>  = NoGhcExts
>  | UseGhcExts String   -- imports
>               [String] -- language extensions

---
this is where the exts matter

> show_st :: GhcExts -> {-State-}Int -> String
> show_st UseGhcExts{} = (++"#") . show
> show_st NoGhcExts    = show

---

> type DebugMode = Bool
> type Options = (DecodeOption, FilterOption, GhcExts)


%-----------------------------------------------------------------------------
"produceGLRParser" generates the files containing the Tomita parsing code.
It produces two files - one for the data (small template), and one for
the driver and data strs (large template).

> produceGLRParser
>        :: (String      -- Base Template
>           ,String)     -- Lib template
>        -> String        -- Root of Output file name
>        -> (ActionTable
>           ,GotoTable)   -- LR tables
>        -> String        -- Start parse function name
>        -> Maybe String  -- Module header
>        -> Maybe String  -- User-defined stuff (token DT, lexer etc.)
>        -> (DebugMode,Options)       -- selecting code-gen style
>        -> Grammar       -- Happy Grammar
>        -> Pragmas       -- Pragmas in the .y-file
>        -> (String       -- data
>           ,String)      -- parser
>
> produceGLRParser (base, lib) basename tables start header trailer (debug,options) g pragmas
>  = ( content base $ ""
>    , lib_content lib
>    )
>  where
>   (imps, lang_exts) = case ghcExts_opt of
>     UseGhcExts is os -> (is, os)
>     _                -> ("", [])
>
>   defines = concat
>      [ [ "HAPPY_DEBUG" | debug ]
>      , [ "HAPPY_GHC"   | UseGhcExts _ _ <- return ghcExts_opt ]
>      ]
>   (_,_,ghcExts_opt) = options

Extract the module name from the given module declaration, if it exists.

>   m_mod_decl = find isModKW . zip [0..] . tails . (' ':) =<< header
>   isModKW (_, c0:'m':'o':'d':'u':'l':'e':c1:_) = not (validIDChar c0 || validIDChar c1)
>   isModKW _                                    = False
>   validIDChar c      = isAlphaNum c || c `elem` "_'"
>   validModNameChar c = validIDChar c || c == '.'
>   data_mod = mod_name ++ "Data"
>   mod_name = case m_mod_decl of
>     Just (_, md) -> takeWhile validModNameChar (dropWhile (not . validModNameChar) (drop 8 md))

Or use a default based upon the filename (original behaviour).

>     Nothing      -> reverse . takeWhile (`notElem` "\\/") $ reverse basename

Remove the module declaration from the header so that the remainder of
the header can be used in the generated code.

>   header_sans_mod = flip (maybe header) m_mod_decl $ \ (mi, _) -> do
>       hdr <- header

Extract the string that comes before the module declaration...

>       let (before, mod_decl) = splitAt mi hdr

>       let isWhereKW (c0:'w':'h':'e':'r':'e':c1:_) = not (validIDChar c0 || validIDChar c1)
>           isWhereKW _ = False
>       let where_after = dropWhile (not . isWhereKW) . tails . (++ "\n") $ mod_decl
>       let after = drop 6 . concat . take 1 $ where_after

...and combine it with the string that comes after the 'where' keyword.

>       return $ before ++ "\n" ++ after

>   (sem_def, sem_info) = mkGSemType options g pragmas
>   table_text = mkTbls tables sem_info (ghcExts_opt) g

>   header_parts = fmap (span (\x -> take 3 (dropWhile isSpace x) == "{-#")
>                                  . lines)
>                       header_sans_mod
>       -- Split off initial options, if they are present
>       -- Assume these options ONLY related to code which is in
>       --   parser tail or in sem. rules

>   content base_defs
>    = str (unlines
>            [ "{-# LANGUAGE " ++ l ++ " #-}\n" | l <- lang_exts ])
>    . str (unlines $ maybe [] fst header_parts) .nl
>    . nl
>    . str (comment "data")                      .nl .nl
>    . str ("module " ++ data_mod ++ " where")   .nl

>     . nl
>     . maybestr (fmap (unlines.snd) header_parts) .nl
>     . nl
>     . str base_defs .nl
>     . nl

>    . let count_nls     = length . filter (=='\n')
>          pre_trailer   = maybe 0 count_nls header_sans_mod -- check fmt below
>                        + count_nls base_defs
>                        + 10                           -- for the other stuff
>          post_trailer  = pre_trailer + maybe 0 count_nls trailer + 4
>      in
>         str ("{-# LINE " ++ show pre_trailer ++ " "
>                          ++ show (basename ++ "Data.hs") ++ "#-}")
>               -- This should show a location in basename.y -- but Happy
>               -- doesn't pass this info through. But we still avoid being
>               -- told a location in GLR_Base!
>       . nl
>       . nl
>       . maybestr trailer
>       .nl
>       .nl
>       . str ("{-# LINE " ++ show post_trailer ++ " "
>                          ++ show (basename ++ "Data.hs") ++ "#-}")
>       . nl
>       . nl

>     . mkGSymbols g pragmas .nl
>     . nl
>     . sem_def                     .nl
>     . nl
>     . mkSemObjects  options (monad_sub pragmas) sem_info  .nl
>     . nl
>     . mkDecodeUtils options (monad_sub pragmas) sem_info  .nl
>     . nl
>     . user_def_token_code (token_type pragmas)            .nl
>     . nl
>     . table_text

>   lib_content lib_text
>    = let (pre,_drop_me : post) = break (== "fakeimport DATA") $ lines lib_text
>      in
>      unlines [ "{-# LANGUAGE CPP #-}"
>              , unlines
>                  [ "#define " ++ d ++ " 1" | d <- defines ]
>              , unlines
>                  [ "{-# LANGUAGE " ++ l ++ " #-}\n" | l <- lang_exts ]
>              , comment "driver" ++ "\n"
>              , "module " ++ mod_name ++ "("
>              , case lexer pragmas of
>                  Nothing     -> ""
>                  Just (lf,_) -> "  " ++ lf ++ ","
>              , "  " ++ start
>              , ""
>              , unlines pre
>              , imps
>              , "import " ++ data_mod
>              , start ++ " = glr_parse "
>              , "use_filtering = " ++ show use_filtering
>              , "top_symbol = " ++ prefix ++ start_prod
>              , unlines post
>              ]
>   start_prod = token_names g ! (let (_,_,i,_) = head $ starts g in i)
>   use_filtering = case options of (_, UseFiltering,_) -> True
>                                   _                   -> False

> comment :: String -> String
> comment which
>  = "-- parser (" ++ which ++ ") produced by Happy (GLR) Version " ++
>       showVersion version

> user_def_token_code :: String -> String -> String
> user_def_token_code tokenType
>  = str "type UserDefTok = " . str tokenType                     . nl
>  . str "instance TreeDecode " . brack tokenType . str " where"  . nl
>  . str "  decode_b f (Branch (SemTok t) []) = [happy_return t]" . nl
>  . str "instance LabelDecode " . brack tokenType . str " where" . nl
>  . str "  unpack (SemTok t) = t"                                . nl


%-----------------------------------------------------------------------------
Formats the tables as code.

> mkTbls :: (ActionTable        -- Action table from Happy
>           ,GotoTable)         -- Goto table from Happy
>        -> SemInfo             -- info about production mapping
>        -> GhcExts             -- Use unboxed values?
>        -> Grammar             -- Happy Grammar
>        -> ShowS
>
> mkTbls (action,goto) sem_info exts g
>  = let gsMap = mkGSymMap g
>        semfn_map = mk_semfn_map sem_info
>    in
>      writeActionTbl action gsMap (semfn_map !) exts g
>    . writeGotoTbl   goto   gsMap exts


%-----------------------------------------------------------------------------
Create a mapping of Happy grammar symbol integers to the data representation
that will be used for them in the GLR parser.

> mkGSymMap :: Grammar -> [(Name,String)]
> mkGSymMap g
>  =    [ -- (errorTok, prefix ++ "Error")
>       ]
>    ++ [ (i, prefix ++ (token_names g) ! i)
>       | i <- user_non_terminals g ]   -- Non-terminals
>    ++ [ (i, "HappyTok (" ++ mkMatch tok ++ ")")
>       | (i,tok) <- token_specs g ]    -- Tokens (terminals)
>    ++ [(eof_term g,"HappyEOF")]       -- EOF symbol (internal terminal)
>  where
>   mkMatch tok = case mapDollarDollar tok of
>                   Nothing -> tok
>                   Just fn -> fn "_"

> toGSym :: [(Int, String)] -> Int -> String
> toGSym gsMap i
>  = case lookup i gsMap of
>     Nothing -> error $ "No representation for symbol " ++ show i
>     Just g  -> g


%-----------------------------------------------------------------------------
Take the ActionTable from Happy and turn it into a String representing a
function that can be included as the action table in the GLR parser.
It also shares identical reduction values as CAFs

> writeActionTbl
>  :: ActionTable -> [(Int,String)] -> (Name->String)
>                                       -> GhcExts -> Grammar -> ShowS
> writeActionTbl acTbl gsMap semfn_map exts g
>  = interleave "\n"
>  $ map str
>  $ mkLines ++ [errorLine] ++ mkReductions
>  where
>   name      = "action"
>   mkLines   = concatMap (mkState) (assocs acTbl)
>   errorLine = name ++ " _ _ = Error"
>   mkState (i,arr)
>    = filter (/="") $ map (mkLine i) (assocs arr)
>
>   mkLine state (symInt,action)
>    | symInt == errorTok       -- skip error productions
>    = ""                       -- NB see ProduceCode's handling of these
>    | otherwise
>    = case action of
>       LR'Fail     -> ""
>       LR'MustFail -> ""
>       _           -> unwords [ startLine , mkAct action ]
>    where
>     startLine
>      = unwords [ name , show_st exts state, "(" , getTok , ") =" ]
>     getTok = let tok = toGSym gsMap symInt
>              in case mapDollarDollar tok of
>                   Nothing -> tok
>                   Just f  -> f "_"
>   mkAct act
>    = case act of
>       LR'Shift newSt _ -> "Shift " ++ show newSt ++ " []"
>       LR'Reduce r    _ -> "Reduce " ++ "[" ++ mkRed r ++ "]"
>       LR'Accept        -> "Accept"
>       LR'Multiple rs (LR'Shift st _)
>                        -> "Shift " ++ show st ++ " " ++ mkReds rs
>       LR'Multiple rs r@(LR'Reduce{})
>                        -> "Reduce " ++ mkReds (r:rs)
>       _ -> error "writeActionTbl/mkAct: Unhandled case"
>    where
>     mkReds rs = "[" ++ tail (concat [ "," ++ mkRed r | LR'Reduce r _ <- rs ]) ++ "]"

>   mkRed r = "red_" ++ show r
>   mkReductions = [ mkRedDefn p
>                  | p@(_, Production n _ _ _) <- zip [0..] $ productions g
>                  , n `notElem` start_productions g ]

>   mkRedDefn (r, Production lhs_id rhs_ids (_code,_dollar_vars) _)
>    = mkRed r ++ " = ("++ lhs ++ "," ++ show arity ++ " :: Int," ++ sem ++")"
>      where
>         lhs = toGSym gsMap $ lhs_id
>         arity = length rhs_ids
>         sem = semfn_map r


%-----------------------------------------------------------------------------
Do the same with the Happy goto table.

> writeGotoTbl :: GotoTable -> [(Int,String)] -> GhcExts -> ShowS
> writeGotoTbl goTbl gsMap exts
>  = interleave "\n" (map str $ filter (not.null) mkLines)
>  . str errorLine . nl
>  where
>   name    = "goto"
>   errorLine = "goto _ _ = " ++ show_st exts (negate 1)
>   mkLines = map mkState (assocs goTbl)
>
>   mkState (i,arr)
>    = unlines $ filter (/="") $ map (mkLine i) (assocs arr)
>
>   mkLine state (ntInt,goto)
>    = case goto of
>       NoGoto  -> ""
>       Goto st -> unwords [ startLine , show_st exts st ]
>    where
>     startLine
>      = unwords [ name , show_st exts state, getGSym , "=" ]
>     getGSym = toGSym gsMap ntInt


%-----------------------------------------------------------------------------
Create the 'GSymbol' ADT for the symbols in the grammar

> mkGSymbols :: Grammar -> Pragmas -> ShowS
> mkGSymbols g pragmas
>  = str dec
>  . str eof
>  . str tok
>  . interleave "\n" [ str " | " . str prefix . str sym . str " "
>                    | sym <- syms ]
>  . str der
>    -- ++ eq_inst
>    -- ++ ord_inst
>  where
>   dec  = "data GSymbol"
>   eof  = " = HappyEOF"
>   tok  = " | HappyTok {-!Int-} (" ++ token_type pragmas ++ ")"
>   der  = "   deriving (Show,Eq,Ord)"
>   syms = [ token_names g ! i | i <- user_non_terminals g ]

NOTES:
Was considering avoiding use of Eq/Ord over tokens, but this then means
hand-coding the Eq/Ord classes since we're over-riding the usual order
except in one case.

maybe possible to form a union and do some juggling, but this isn't that
easy, eg input type of "action".

plus, issues about how token info gets into TreeDecode sem values - which
might be tricky to arrange.
<>   eq_inst = "instance Eq GSymbol where"
<>           : "  HappyTok i _ == HappyTok j _ = i == j"
<>           : [ "  i == j = fromEnum i == fromEnum j"



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Semantic actions on rules.

These are stored in a union type "GSem", and the semantic values are held
on the branches created at the appropriate reduction.

"GSem" type has one constructor per distinct type of semantic action and
pattern of child usage.


%-----------------------------------------------------------------------------
Creating a type for storing semantic rules
 - also collects information on code structure and constructor names, for
   use in later stages.

> type SemInfo
>  = [(String, String, [Int], [((Int,Int), ([(Int,String)],String), [Int])])]

> mkGSemType :: Options -> Grammar -> Pragmas -> (ShowS, SemInfo)
> mkGSemType (TreeDecode,_,_) g pragmas
>  = (def, map snd syms)
>  where
>   mtype s = case monad_sub pragmas of
>               Nothing       -> s
>               Just (ty,_,_) -> ty ++ ' ' : brack s ""

>   def  = str "data GSem" . nl
>        . str " = NoSem"  . nl
>        . str (" | SemTok (" ++  token_type pragmas ++ ")") . nl
>        . interleave "\n" [ str " | " . str sym . str " "
>                          | sym <- map fst syms ]
>        . str "instance Show GSem where" . nl
>        . interleave "\n" [ str "  show " . str c . str "{} = " . str (show c)
>                          | (_,c,_,_) <- map snd syms ]

>   syms = [ (c_name ++ " (" ++ ty ++ ")", (rty, c_name, mask, prod_info))
>          | (i,this@(mask,args,rty)) <- zip [0..] (nub $ map fst info)
>                                               -- find unique types (plus mask)
>          , let c_name = "Sem_" ++ show i
>          , let mrty = mtype rty
>          , let ty = foldr (\l r -> l ++ " -> " ++ r) mrty args

>          , let code_info = [ j_code | (that, j_code) <- info, this == that ]
>          , let prod_info = [ ((i,k), code, js)
>                            | (k,code) <- zip [0..] (nub $ map snd code_info)
>                            , let js = [ j | (j,code2) <- code_info
>                                           , code == code2 ]
>                            ]
>            -- collect specific info about productions with this type
>          ]

>   info = [ ((var_mask, args, i_ty), (j,(ts_pats,code)))
>          | i <- user_non_terminals g
>          , let i_ty = typeOf i
>          , j <- lookupProdsOfName g i  -- all prod numbers
>          , let Production _ ts (raw_code,dollar_vars) _ = lookupProdNo g j
>          , let var_mask = map (\x -> x - 1) vars_used
>                           where vars_used = sort $ nub dollar_vars
>          , let args = [ typeOf $ ts !! v | v <- var_mask ]
>          , let code | all isSpace raw_code = "()"
>                     | otherwise            = raw_code
>          , let ts_pats = [ (k+1,c) | k <- var_mask
>                                    , (t,c) <- token_specs g
>                                    , ts !! k == t ]
>          ]

>   typeOf n | n `elem` terminals g = token_type pragmas
>            | otherwise            = case types g ! n of
>                                       Nothing -> "()"         -- default
>                                       Just t  -> t

> -- NB expects that such labels are Showable
> mkGSemType (LabelDecode,_,_) g pragmas
>  = (def, map snd syms)
>  where
>   def = str "data GSem" . nl
>       . str " = NoSem"  . nl
>       . str (" | SemTok (" ++  token_type pragmas ++ ")")
>       . interleave "\n" [ str " | "  . str sym . str " "
>                         | sym <- map fst syms ]
>       . str "   deriving (Show)" . nl

>   syms = [ (c_name ++ " (" ++ ty ++ ")", (ty, c_name, mask, prod_info))
>          | (i,this@(mask,ty)) <- zip [0..] (nub $ map fst info)
>                                               -- find unique types
>          , let c_name = "Sem_" ++ show i
>          , let code_info = [ j_code | (that, j_code) <- info, this == that ]
>          , let prod_info = [ ((i,k), code, js)
>                            | (k,code) <- zip [0..] (nub $ map snd code_info)
>                            , let js = [ j | (j,code2) <- code_info
>                                           , code == code2 ]

>                            ]
>            -- collect specific info about productions with this type
>          ]

>   info = [ ((var_mask,i_ty), (j,(ts_pats,code)))
>          | i <- user_non_terminals g
>          , let i_ty = typeOf i
>          , j <- lookupProdsOfName g i  -- all prod numbers
>          , let Production _ ts (code,dollar_vars) _ = lookupProdNo g j
>          , let var_mask = map (\x -> x - 1) vars_used
>                           where vars_used = sort $ nub dollar_vars
>          , let ts_pats = [ (k+1,c) | k <- var_mask
>                                    , (t,c) <- token_specs g
>                                    , ts !! k == t ]
>          ]

>   typeOf n = case types g ! n of
>                Nothing -> "()"                -- default
>                Just t  -> t


%---------------------------------------
Creates the appropriate semantic values.
 - for label-decode, these are the code, but abstracted over the child indices
 - for tree-decode, these are the code abstracted over the children's values

> mkSemObjects :: Options -> MonadInfo -> SemInfo -> ShowS
> mkSemObjects (LabelDecode,filter_opt,_) _ sem_info
>  = interleave "\n"
>  $ [   str (mkSemFn_Name ij)
>      . str (" ns@(" ++ pat ++ "happy_rest) = ")
>      . str (" Branch (" ++ c_name ++ " (" ++ code ++ ")) ")
>      . str (nodes filter_opt)
>    | (_ty, c_name, mask, prod_info) <- sem_info
>    , (ij, (pats,code), _ps) <- prod_info
>    , let pat | null mask = ""
>              | otherwise = concatMap (\v -> mk_tok_binder pats (v+1) ++ ":")
>                                      [0..maximum mask]

>    , let nodes NoFiltering  = "ns"
>          nodes UseFiltering = "(" ++ foldr (\l -> mkHappyVar (l+1) . showChar ':') "[])" mask
>    ]
>    where
>       mk_tok_binder pats v
>        = mk_binder (\s -> "(_,_,HappyTok (" ++ s ++ "))") pats v ""


> mkSemObjects (TreeDecode,filter_opt,_) monad_info sem_info
>  = interleave "\n"
>  $ [   str (mkSemFn_Name ij)
>      . str (" ns@(" ++ pat ++ "happy_rest) = ")
>      . str (" Branch (" ++ c_name ++ " (" ++ sem ++ ")) ")
>      . str (nodes filter_opt)
>    | (_ty, c_name, mask, prod_info) <- sem_info
>    , (ij, (pats,code), _) <- prod_info
>    , let indent c = init $ unlines $ map (replicate 4 ' '++) $ lines c
>    , let mcode = case monad_info of
>                    Nothing -> code
>                    Just (_,_,rtn) -> case code of
>                                        '%':code' -> "\n" ++ indent code'
>                                        _         -> rtn ++ " (" ++ code ++ ")"
>    , let sem = foldr (\v t -> mk_lambda pats (v + 1) "" ++ t) mcode mask
>    , let pat | null mask = ""
>              | otherwise = concatMap (\v -> mkHappyVar (v+1) ":")
>                                      [0..maximum mask]
>    , let nodes NoFiltering  = "ns"
>          nodes UseFiltering = "(" ++ foldr (\l -> mkHappyVar (l+1) . showChar ':') "[])" mask
>    ]

> mk_lambda :: [(Int, String)] -> Int -> String -> String
> mk_lambda pats v
>  = (\s -> "\\" ++ s ++ " -> ") . mk_binder id pats v

> mk_binder :: (String -> String) -> [(Int, String)] -> Int -> String -> String
> mk_binder wrap pats v
>  = case lookup v pats of
>       Nothing -> mkHappyVar v
>       Just p  -> case mapDollarDollar p of
>                     Nothing -> wrap . mkHappyVar v . showChar '@' . brack p
>                     Just fn -> wrap . brack' (fn . mkHappyVar v)


---
standardise the naming scheme

> mkSemFn_Name :: (Int, Int) -> String
> mkSemFn_Name (i,j) = "semfn_" ++ show i ++ "_" ++ show j

---
maps production name to the underlying (possibly shared) semantic function

> mk_semfn_map :: SemInfo -> Array Name String
> mk_semfn_map sem_info
>  = array (0,maximum $ map fst prod_map) prod_map
>    where
>        prod_map = [ (p, mkSemFn_Name ij)
>                   | (_,_,_,pi') <- sem_info, (ij,_,ps) <- pi', p <- ps ]


%-----------------------------------------------------------------------------
Create default decoding functions

Idea is that sem rules are stored as functions in the AbsSyn names, and
only unpacked when needed. Using classes here to manage the unpacking.

> mkDecodeUtils :: Options -> MonadInfo -> SemInfo -> ShowS
> mkDecodeUtils (TreeDecode,filter_opt,_) monad_info seminfo
>  = interleave "\n"
>  $ map str (monad_defs monad_info)
>    ++ map mk_inst ty_cs
>    where
>       ty_cs = [ (ty, [ (c_name, mask)
>                      | (ty2, c_name, mask, _j_vs) <- seminfo
>                      , ty2 == ty
>                      ])
>               | ty <- nub [ ty | (ty,_,_,_) <- seminfo ]
>               ]               -- group by same type

>       mk_inst (ty, cs_vs)
>        = str ("instance TreeDecode (" ++ ty ++ ") where ") . nl
>        . interleave "\n"
>          [   str "  "
>            . str ("decode_b f (Branch (" ++ c_name ++ " s)")
>            . str (" (" ++ var_pat ++ ")) = ")
>            . cross_prod monad_info "s" (nodes filter_opt)
>          | (c_name, vs) <- cs_vs
>          , let vars = [ "b_" ++ show n | n <- var_range filter_opt vs ]
>          , let var_pat = foldr (\l r -> l ++ ":" ++ r) "_" vars
>          , let nodes NoFiltering  = [ vars !! n | n <- vs ]
>                nodes UseFiltering = vars
>          ]

>       var_range _            [] = []
>       var_range NoFiltering  vs = [0 .. maximum vs ]
>       var_range UseFiltering vs = [0 .. length vs - 1]

>       cross_prod Nothing s_var nodes
>        = cross_prod_ (char '[' . str s_var . char ']')
>                      (map str nodes)
>       cross_prod (Just (_,_,rtn)) s_var nodes
>        = str "map happy_join $ "
>        . cross_prod_ (char '[' . str rtn . char ' ' . str s_var . char ']')
>                      (map str nodes)

>       cross_prod_ = foldl (\s a -> brack'
>                                  $ str "cross_fn"
>                                  . char ' ' . s
>                                  . str " $ decode f "
>                                  . a)



> mkDecodeUtils (LabelDecode,_,_) monad_info seminfo
>  = interleave "\n"
>  $ map str
>  $ monad_defs monad_info ++ concatMap (mk_inst) ty_cs
>    where
>       ty_cs = [ (ty, [ (c_name, mask)
>                      | (ty2, c_name, mask, _) <- seminfo
>                      , ty2 == ty
>                      ])
>               | ty <- nub [ ty | (ty,_,_,_) <- seminfo ]
>               ]               -- group by same type

>       mk_inst (ty, cns)
>        = ("instance LabelDecode (" ++ ty ++ ") where ")
>        : [ "  unpack (" ++ c_name ++ " s) = s"
>          | (c_name, _mask) <- cns ]


---
This selects the info used for monadic parser generation

> type MonadInfo = Maybe (String,String,String)
> monad_sub :: Pragmas -> MonadInfo
> monad_sub pragmas
>  = case monad pragmas of
>      (True, _, ty,bd,ret) -> Just (ty,bd,ret)
>      _                    -> Nothing
>    -- TMP: only use monad info if it was user-declared, and ignore ctxt
>    -- TMP: otherwise default to non-monadic code
>    -- TMP: (NB not sure of consequences of monads-everywhere yet)


---
form the various monad-related defs.

> monad_defs :: MonadInfo -> [String]
> monad_defs Nothing
>  = [ "type Decode_Result a = a"
>    , "happy_ap = ($)"
>    , "happy_return = id"]
> monad_defs (Just (ty,tn,rtn))
>  = [ "happy_join x = (" ++ tn ++ ") x id"
>    , "happy_ap f a = (" ++ tn ++ ") f (\\f -> (" ++ tn ++ ") a (\\a -> " ++ rtn ++ "(f a)))"
>    , "type Decode_Result a = " ++ brack ty " a"
>    , "happy_return = " ++ rtn ++ " :: a -> Decode_Result a"
>    ]

%-----------------------------------------------------------------------------
Util Functions

---
remove Happy-generated start symbols.

> user_non_terminals :: Grammar -> [Name]
> user_non_terminals g
>  = non_terminals g \\ start_productions g

> start_productions :: Grammar -> [Name]
> start_productions g = [ s | (_,s,_,_) <- starts g ]


---

> mkHappyVar :: Int -> String -> String
> mkHappyVar n = str "happy_var_" . shows n

%------------------------------------------------------------------------------
Fast string-building functions

> str :: String -> String -> String
> str = showString
> char :: Char -> String -> String
> char c = (c :)
> interleave :: String -> [String -> String] -> String -> String
> interleave s = foldr (\a b -> a . str s . b) id

> nl :: String -> String
> nl = char '\n'

> maybestr :: Maybe String -> String -> String
> maybestr (Just s)     = str s
> maybestr _            = id

> brack :: String -> String -> String
> brack s = str ('(' : s) . char ')'
> brack' :: (String -> String) -> String -> String
> brack' s = char '(' . s . char ')'
