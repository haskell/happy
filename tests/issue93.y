-- See <https://github.com/haskell/happy/issues/93> for more information
-- This is an example of a grammar that has more than 2^15 entries in `happyTable` (39817).
{
import System.Exit
import Data.Char
}

%name parseLit lit
%name parseAttr export_attribute
%name parseTy export_ty
%name parsePat pat
%name parseStmt stmt
%name parseExpr expr
%name parseItem mod_item
%name parseSourceFileContents source_file
%name parseBlock export_block
%name parseImplItem impl_item
%name parseTraitItem trait_item
%name parseTt token_tree
%name parseTokenStream token_stream
%name parseTyParam ty_param
%name parseLifetimeDef lifetime_def
%name parseWhereClause where_clause
%name parseGenerics generics

%tokentype { Token }
%lexer { lexNonSpace `bindP` } { Eof }
%monad { P } { bindP } { returnP }

%error { parseError }

%expect 0

%token


  '='            { Equal }
  '<'            { Less }
  '>'            { Greater }
  '!'            { Exclamation }
  '~'            { Tilde }

  '+'            { Plus }
  '-'            { Minus }
  '*'            { Star }
  '/'            { Slash }
  '%'            { Percent }
  '^'            { Caret } 
  '&'            { Ampersand }
  '|'            { Pipe }


  '@'            { At }
  '...'          { DotDotDot }
  '..'           { DotDot }
  '.'            { Dot }
  ','            { Comma }
  ';'            { Semicolon }
  '::'           { ModSep }
  ':'            { Colon }
  '->'           { RArrow }
  '<-'           { LArrow }
  '=>'           { FatArrow }
  '#'            { Pound }
  '$'            { Dollar }
  '?'            { Question }
  '#!'           { Shebang }

  '||'           { PipePipe }
  '&&'           { AmpersandAmpersand }
  '>='           { GreaterEqual }
  '>>='          { GreaterGreaterEqual }
  '<<'           { LessLess }
  '>>'           { GreaterGreater }

  '=='           { EqualEqual }
  '!='           { NotEqual }
  '<='           { LessEqual }
  '<<='          { LessLessEqual }
  '-='           { MinusEqual }
  '&='           { AmpersandEqual }
  '|='           { PipeEqual }
  '+='           { PlusEqual }
  '*='           { StarEqual }
  '/='           { SlashEqual }
  '^='           { CaretEqual }
  '%='           { PercentEqual }

  '('            { OpenParen }
  '['            { OpenBracket }
  '{'            { OpenBrace }
  ')'            { CloseParen }
  ']'            { CloseBracket }
  '}'            { CloseBrace }


  byte           { ByteTok{} }
  char           { CharTok{} }
  int            { IntegerTok{} }
  float          { FloatTok{} }
  str            { StrTok{} }
  byteStr        { ByteStrTok{} }
  rawStr         { StrRawTok{} }
  rawByteStr     { ByteStrRawTok{} }


  as             { IdentTok "as" }
  box            { IdentTok "box" }
  break          { IdentTok "break" }
  const          { IdentTok "const" }
  continue       { IdentTok "continue" }
  crate          { IdentTok "crate" }
  else           { IdentTok "else" }
  enum           { IdentTok "enum" }
  extern         { IdentTok "extern" }
  false          { IdentTok "false" }
  fn             { IdentTok "fn" }
  for            { IdentTok "for" }
  if             { IdentTok "if" }
  impl           { IdentTok "impl" }
  in             { IdentTok "in" }
  let            { IdentTok "let" }
  loop           { IdentTok "loop" }
  match          { IdentTok "match" }
  mod            { IdentTok "mod" }
  move           { IdentTok "move" }
  mut            { IdentTok "mut" }
  pub            { IdentTok "pub" }
  ref            { IdentTok "ref" }
  return         { IdentTok "return" }
  Self           { IdentTok "Self" }
  self           { IdentTok "self" }
  static         { IdentTok "static" }
  struct         { IdentTok "struct" }
  super          { IdentTok "super" }
  trait          { IdentTok "trait" }
  true           { IdentTok "true" }
  type           { IdentTok "type" }
  unsafe         { IdentTok "unsafe" }
  use            { IdentTok "use" }
  where          { IdentTok "where" }
  while          { IdentTok "while" }
  do             { IdentTok "do" }

  abstract       { IdentTok "abstract" }
  alignof        { IdentTok "alignof" }
  become         { IdentTok "become" }
  final          { IdentTok "final" }
  macro          { IdentTok "macro" }
  offsetof       { IdentTok "offsetof" }
  override       { IdentTok "override" }
  priv           { IdentTok "priv" }
  proc           { IdentTok "proc" }
  pure           { IdentTok "pure" }
  sizeof         { IdentTok "sizeof" }
  typeof         { IdentTok "typeof" }
  unsized        { IdentTok "unsized" }
  virtual        { IdentTok "virtual" }
  yield          { IdentTok "yield" }


  default        { IdentTok "default" }
  union          { IdentTok "union" }
  catch          { IdentTok "catch" }


  outerDoc       { OuterDoc }
  innerDoc       { InnerDoc }


  IDENT          { IdentTok{} }
  '_'            { Underscore }


  LIFETIME       { LifetimeTok _ }


  ntItem         { Interpolated 0 }
  ntBlock        { Interpolated 1 } 
  ntStmt         { Interpolated 2 } 
  ntPat          { Interpolated 3 }
  ntExpr         { Interpolated 4 }
  ntTy           { Interpolated 5 }
  ntIdent        { Interpolated 6 }
  ntPath         { Interpolated 7 }
  ntTT           { Interpolated 8 }
  ntArm          { Interpolated 9 }
  ntImplItem     { Interpolated 10 }
  ntTraitItem    { Interpolated 11 }
  ntGenerics     { Interpolated 12 }
  ntWhereClause  { Interpolated 13 }
  ntArg          { Interpolated 14 }
  ntLit          { Interpolated 15 }

%nonassoc SEG
%nonassoc mut DEF EQ '::'
%nonassoc IDENT ntIdent default union catch self 
%nonassoc box return break continue IMPLTRAIT LAMBDA
%right '=' '>>=' '<<=' '-=' '+=' '*=' '/=' '^=' '|=' '&=' '%='
%right '<-'
%nonassoc SINGLERNG
%nonassoc INFIXRNG
%nonassoc POSTFIXRNG
%nonassoc PREFIXRNG
%nonassoc '..' '...'
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%left '|'
%left '^'
%left '&'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%nonassoc ':' as
%nonassoc UNARY
%nonassoc FIELD VIS PATH WHERE NOSEMI
%nonassoc '?' '.'
%nonassoc '{' ntBlock '[' '(' '!' ';'

%%

ident :: { Int }
  : ntIdent                                                                   { 0 } 
  | union                                                                     { 1 } 
  | default                                                                   { 2 } 
  | catch                                                                     { 3 } 
  | IDENT                                                                     { 4 }

gt :: { Int }
  : {- empty -}                                                               { 5 }

some(p) :: { Int }
  : some(p) p                                                                 { 6 } 
  | p                                                                         { 7 } 

many(p) :: { Int }
  : some(p)                                                                   { 8 }
  | {- empty -}                                                               { 9 }

sep_by1(p,sep) :: { Int }
  : sep_by1(p,sep) sep p                                                      { 10 }
  | p                                                                         { 11 }

sep_by(p,sep) :: { Int }
  : sep_by1(p,sep)                                                            { 12 }
  | {- empty -}                                                               { 13 }

sep_by1T(p,sep) :: { Int }
  : sep_by1(p,sep) sep                                                        { 14 }
  | sep_by1(p,sep)                                                            { 15 }

sep_byT(p,sep) :: { Int }
  : sep_by1T(p,sep)                                                           { 16 }
  | {- empty -}                                                               { 17 }

source_file :: { Int }
  : inner_attrs many(mod_item)                                                { 18 }
  |             many(mod_item)                                                { 19 } 

outer_attribute :: { Int }
  : '#' '[' mod_path token_stream ']'                                         { 20 } 
  | outerDoc                                                                  { 21 }

inner_attribute :: { Int }
  : '#' '!' '[' mod_path token_stream ']'                                     { 22 }
  | '#!'    '[' mod_path token_stream ']'                                     { 23 }
  | innerDoc                                                                  { 24 }

inner_attrs :: { Int }
  : inner_attrs inner_attribute                                               { 25 }
  | inner_attribute                                                           { 26 }

lit :: { Int }
  : ntLit                                                                     { 27 }
  | byte                                                                      { 28 }
  | char                                                                      { 29 }
  | int                                                                       { 30 }
  | float                                                                     { 31 }
  | true                                                                      { 32 }
  | false                                                                     { 33 }
  | string                                                                    { 34 }

string :: { Int }
  : str                                                                       { 35 }
  | rawStr                                                                    { 36 }
  | byteStr                                                                   { 37 }
  | rawByteStr                                                                { 38 }

qual_path(segs) :: { Int }
  : '<' qual_path_suf(segs)                                                   { 39 }
  | lt_ty_qual_path as ty_path '>' '::' segs                                  { 40 }

qual_path_suf(segs) :: { Int }
  : ty '>' '::' segs                                                          { 41 }
  | ty as ty_path '>' '::' segs                                               { 42 }

lt_ty_qual_path :: { Int }
  : '<<' qual_path_suf(path_segments_without_colons)                          { 43 }

generic_values :: { Int }
  : '<' sep_by1(lifetime,',')  ',' sep_by1T(ty,',')            gt '>'         { 45 }
  | '<' sep_by1(lifetime,',')  ','       sep_by1T(binding,',') gt '>'         { 46 }
  | '<' sep_by1T(lifetime,',')                                 gt '>'         { 47 }
  | '<'              sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'         { 48 }
  | '<'              sep_by1T(ty,',')                          gt '>'         { 49 }
  | '<'                                  sep_by1T(binding,',') gt '>'         { 50 }
  | '<'                                                        gt '>'         { 51 }
  | lt_ty_qual_path ',' sep_by1T(ty,',')                       gt '>'         { 53 }
  | lt_ty_qual_path                  ',' sep_by1T(binding,',') gt '>'         { 54 }
  | lt_ty_qual_path                                            gt '>'         { 55 }

binding :: { Int }
  : ident '=' ty                                                              { 56 }

ty_path :: { Int }
  : ntPath                                                                    { 57 }
  | path_segments_without_colons                                              { 58 }
  | '::' path_segments_without_colons                                         { 59 }

ty_qual_path :: { Int }
  : qual_path(path_segments_without_colons)                                   { 60 }

path_segments_without_colons :: { Int }
  : sep_by1(path_segment_without_colons, '::') %prec SEG                      { 61 }

path_segment_without_colons :: { Int }
  : self_or_ident path_parameter1                                             { 62 }

path_parameter1 :: { Int }
  : generic_values                                                            { 63 }
  | '(' sep_byT(ty,',') ')'                                                   { 64 }
  | '(' sep_byT(ty,',') ')' '->' ty_no_plus                                   { 65 }
  | {- empty -}                  %prec IDENT                                  { 66 }

expr_path :: { Int }
  : ntPath                                                                    { 67 }
  | path_segments_with_colons                                                 { 68 }
  | '::' path_segments_with_colons                                            { 69 }

expr_qual_path :: { Int }
  : qual_path(path_segments_with_colons)                                      { 70 }

path_segments_with_colons :: { Int }
  : self_or_ident                                                             { 71 }
  | path_segments_with_colons '::' self_or_ident                              { 72 }
  | path_segments_with_colons '::' generic_values                             { 73 }

mod_path :: { Int }
  : ntPath                                                                    { 74 } 
  | self_or_ident                                                             { 75 }
  | '::' self_or_ident                                                        { 76 }
  | mod_path '::' ident                                                       { 77 }

lifetime :: { Int }
  : LIFETIME                                                                  { 78 }

trait_ref :: { Int }
  : ty_path                                                                   { 79 }

ty :: { Int }
  : ty_no_plus                                                                { 80 }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+')             { 81 } 

ty_no_plus :: { Int }
  : ntTy                                                                      { 82 }
  | no_for_ty                                                                 { 83 }
  | for_ty_no_plus                                                            { 84 }

ty_prim :: { Int }
  : no_for_ty_prim                                                            { 85 }
  | for_ty_no_plus                                                            { 86 }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+')             { 87 } 

no_for_ty :: { Int }
  : no_for_ty_prim                                                            { 88 }
  | '(' ')'                                                                   { 89 }
  | '(' ty ')'                                                                { 90 }
  | '(' ty ',' ')'                                                            { 91 }
  | '(' ty ',' sep_by1T(ty,',') ')'                                           { 92 }
  | ty_qual_path                                                              { 93 }

no_for_ty_prim :: { Int }
  : '_'                                                                       { 94 }
  | '!'                                                                       { 95 }
  | '[' ty ']'                                                                { 96 }
  | '*' ty_no_plus                                                            { 97 }
  | '*' const ty_no_plus                                                      { 98 }
  | '*' mut   ty_no_plus                                                      { 99 }
  | '&'               ty_no_plus                                              { 100 }
  | '&'  lifetime     ty_no_plus                                              { 101 }
  | '&'           mut ty_no_plus                                              { 102 }
  | '&'  lifetime mut ty_no_plus                                              { 103 }
  | '&&'              ty_no_plus                                              { 104 }
  | '&&' lifetime     ty_no_plus                                              { 105 }
  | '&&'          mut ty_no_plus                                              { 106 }
  | '&&' lifetime mut ty_no_plus                                              { 107 }
  | ty_path               %prec PATH                                          { 108 } 
  | ty_mac                                                                    { 109 } 
  | unsafe extern abi fn fn_decl(arg_general)                                 { 110 }
  | unsafe fn fn_decl(arg_general)                                            { 111 }
  | extern abi fn fn_decl(arg_general)                                        { 112 }
  | fn fn_decl(arg_general)                                                   { 113 }
  | typeof '(' expr ')'                                                       { 114 }
  | '[' ty ';' expr ']'                                                       { 115 }
  | '?' trait_ref                                                             { 116 }
  | '?' for_lts trait_ref                                                     { 117 }

for_ty_no_plus :: { Int }
  : for_lts unsafe extern abi fn fn_decl(arg_general)                         { 118 } 
  | for_lts unsafe fn fn_decl(arg_general)                                    { 119 }
  | for_lts extern abi fn fn_decl(arg_general)                                { 120 }
  | for_lts fn fn_decl(arg_general)                                           { 121 }
  | for_lts trait_ref                                                         { 122 }

impl_ty :: { Int }
  : impl sep_by1(ty_param_bound_mod,'+') %prec IMPLTRAIT                      { 123 }

lifetime_mut :: { Int }
  : lifetime mut                                                              { 124 }
  | lifetime                                                                  { 125 }
  |          mut                                                              { 126 }
  | {- empty -}                                                               { 127 }

fn_decl(arg) :: { Int }
  : '(' sep_by1(arg,',') ',' '...' ')' ret_ty                                 { 128 }
  | '(' sep_byT(arg,',')           ')' ret_ty                                 { 129 }

fn_decl_with_self_general :: { Int }
  : '(' arg_self_general ',' sep_byT(arg_general,',') ')' ret_ty              { 130 }
  | '(' arg_self_general                              ')' ret_ty              { 131 }
  | '('                                               ')' ret_ty              { 132 }

fn_decl_with_self_named :: { Int }
  : '(' arg_self_named ',' sep_by1(arg_named,',') ',' ')' ret_ty              { 133 }
  | '(' arg_self_named ',' sep_by1(arg_named,',')     ')' ret_ty              { 134 }
  | '(' arg_self_named ','                            ')' ret_ty              { 135 }
  | '(' arg_self_named                                ')' ret_ty              { 136 }
  | fn_decl(arg_named)                                                        { 137 }

ty_param_bound :: { Int }
  : lifetime                                                                  { 138 }
  | poly_trait_ref                                                            { 139 }

poly_trait_ref_mod_bound :: { Int }
  : poly_trait_ref                                                            { 140 }
  | '?' poly_trait_ref                                                        { 141 }

ty_param_bound_mod :: { Int }
  : ty_param_bound                                                            { 142 }
  | '?' poly_trait_ref                                                        { 143 } 

abi :: { Int }
  : str                                                                       { 144 }
  | {- empty -}                                                               { 145 }

ret_ty :: { Int }
  : '->' ty_no_plus                                                           { 146 }
  | '->' impl_ty                                                              { 147 }
  | {- empty -}                                                               { 148 }

poly_trait_ref :: { Int }
  :         trait_ref                                                         { 149 }
  | for_lts trait_ref                                                         { 150 }

for_lts :: { Int }
  : for '<' sep_byT(lifetime_def,',') '>'                                     { 151 }

lifetime_def :: { Int }
  : many(outer_attribute) lifetime ':' sep_by1T(lifetime,'+')                 { 152 }
  | many(outer_attribute) lifetime                                            { 153 }

arg_named :: { Int }
  : ntArg                                                                     { 154 }
  | pat ':' ty                                                                { 155 }

arg_general :: { Int }
  : ntArg                                                                     { 156 } 
  |                ty                                                         { 157 } 
  |      '_'   ':' ty                                                         { 158 } 
  |      ident ':' ty                                                         { 159 } 
  | mut  ident ':' ty                                                         { 160 } 
  | '&'  '_'   ':' ty                                                         { 161 } 
  | '&'  ident ':' ty                                                         { 162 }
  | '&&' '_'   ':' ty                                                         { 163 }
  | '&&' ident ':' ty                                                         { 164 }
  
arg_self_general :: { Int }
  : mut self                                                                  { 165 } 
  |     self ':' ty                                                           { 166 }
  | mut self ':' ty                                                           { 167 }
  | arg_general                                                               { 168 }

arg_self_named :: { Int }
  :                  self                                                     { 169 } 
  |              mut self                                                     { 170 } 
  | '&'              self                                                     { 171 } 
  | '&' lifetime     self                                                     { 172 } 
  | '&'          mut self                                                     { 173 } 
  | '&' lifetime mut self                                                     { 174 }
  |     self ':' ty                                                           { 175 }
  | mut self ':' ty                                                           { 176 }

lambda_arg :: { Int }
  : ntArg                                                                     { 177 }
  | pat ':' ty                                                                { 178 }
  | pat                                                                       { 179 }

pat :: { Int }
  : ntPat                                                                     { 180 }
  | '_'                                                                       { 181 }
  | '&' mut pat                                                               { 182 }
  | '&' pat                                                                   { 183 }
  | '&&' mut pat                                                              { 184 }
  | '&&' pat                                                                  { 185 }
  |     lit_expr                                                              { 186 }
  | '-' lit_expr                                                              { 187 }
  | box pat                                                                   { 188 }
  | binding_mode1 ident '@' pat                                               { 189 }
  | binding_mode1 ident                                                       { 190 }
  |               ident '@' pat                                               { 191 }
  | expr_path                                                                 { 192 }
  | expr_qual_path                                                            { 193 } 
  | lit_or_path '...' lit_or_path                                             { 194 } 
  | expr_path '{' '..' '}'                                                    { 195 } 
  | expr_path '{' pat_fields '}'                                              { 196 } 
  | expr_path '(' pat_tup ')'                                                 { 197 } 
  | expr_mac                                                                  { 198 } 
  | '[' pat_slice ']'                                                         { 199 } 
  | '(' pat_tup ')'                                                           { 200 }

pat_tup :: { Int }
  : sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',')                            { 201 }
  | sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',') ','                        { 202 }
  | sep_by1(pat,',') ',' '..'                                                 { 203 }
  | sep_by1(pat,',')                                                          { 204 }
  | sep_by1(pat,',') ','                                                      { 205 }
  |                      '..' ',' sep_by1(pat,',')                            { 206 }
  |                      '..' ',' sep_by1(pat,',') ','                        { 207 }
  |                      '..'                                                 { 208 }
  | {- empty -}                                                               { 209 }

pat_slice :: { Int }
  : sep_by1(pat,',') ',' '..' ',' sep_by1T(pat,',')                           { 210 }
  | sep_by1(pat,',') ',' '..'                                                 { 211 }
  | sep_by1(pat,',')     '..' ',' sep_by1T(pat,',')                           { 212 }
  | sep_by1(pat,',')     '..'                                                 { 213 }
  |                               sep_by1T(pat,',')                           { 214 }
  |                      '..' ',' sep_by1T(pat,',')                           { 215 }
  |                      '..'                                                 { 216 }
  | {- empty -}                                                               { 217 }

lit_or_path :: { Int }
  : expr_path                                                                 { 218 }
  | expr_qual_path                                                            { 219 }
  | '-' lit_expr                                                              { 220 }
  |     lit_expr                                                              { 221 }

pat_fields :: { Int }
  : sep_byT(pat_field,',')                                                    { 222 }
  | sep_by1(pat_field,',') ',' '..'                                           { 223 }

pat_field :: { Int }
  :     binding_mode ident                                                    { 224 }
  | box binding_mode ident                                                    { 225 }
  |     binding_mode ident ':' pat                                            { 226 }

binding_mode1 :: { Int }
  : ref mut                                                                   { 227 } 
  | ref                                                                       { 228 }
  |     mut                                                                   { 229 }

binding_mode :: { Int }
  : binding_mode1                                                             { 230 }
  | {- empty -}                                                               { 231 }

gen_expression(lhs,rhs,rhs2) :: { Int }
  : ntExpr                                                                    { 232 }
  | lit_expr                                                                  { 233 }
  | '[' sep_byT(expr,',') ']'                                                 { 234 }
  | '[' inner_attrs sep_byT(expr,',') ']'                                     { 235 }
  | '[' expr ';' expr ']'                                                     { 236 }
  | expr_mac                                                                  { 237 }
  | expr_path            %prec PATH                                           { 238 }
  | expr_qual_path                                                            { 239 }
  | '*'      rhs     %prec UNARY                                              { 240 }
  | '!'      rhs     %prec UNARY                                              { 241 }
  | '-'      rhs     %prec UNARY                                              { 242 }
  | '&'      rhs     %prec UNARY                                              { 243 }
  | '&'  mut rhs     %prec UNARY                                              { 244 }
  | '&&'     rhs     %prec UNARY                                              { 245 }
  | '&&' mut rhs     %prec UNARY                                              { 246 }
  | box rhs          %prec UNARY                                              { 247 }
  | left_gen_expression(lhs,rhs,rhs2)                                         { 248 }
  |     '..'  rhs2  %prec PREFIXRNG                                           { 249 }
  |     '...' rhs2  %prec PREFIXRNG                                           { 250 }
  |     '..'        %prec SINGLERNG                                           { 251 }
  |     '...'       %prec SINGLERNG                                           { 252 }
  | return                                                                    { 253 }
  | return rhs                                                                { 254 }
  | continue                                                                  { 255 }
  | continue lifetime                                                         { 256 }
  | break                                                                     { 257 }
  | break          rhs                                                        { 258 }
  | break lifetime                                                            { 259 }
  | break lifetime rhs   %prec break                                          { 260 }
  | move lambda_args rhs   %prec LAMBDA                                       { 261 }
  |      lambda_args rhs   %prec LAMBDA                                       { 262 }

left_gen_expression(lhs,rhs,rhs2) :: { Int }
  : postfix_blockexpr(lhs)                                                    { 263 }
  | lhs '[' expr ']'                                                          { 264 }
  | lhs '(' sep_byT(expr,',') ')'                                             { 265 }
  | lhs ':' ty_no_plus                                                        { 266 }
  | lhs as ty_no_plus                                                         { 267 }
  | lhs '*' rhs                                                               { 268 }
  | lhs '/' rhs                                                               { 269 }
  | lhs '%' rhs                                                               { 270 }
  | lhs '+' rhs                                                               { 271 }
  | lhs '-' rhs                                                               { 272 }
  | lhs '<<' rhs                                                              { 273 }
  | lhs '>>' rhs                                                              { 274 }
  | lhs '&' rhs                                                               { 275 }
  | lhs '^' rhs                                                               { 276 }
  | lhs '|' rhs                                                               { 277 }
  | lhs '==' rhs                                                              { 278 }
  | lhs '!=' rhs                                                              { 279 }
  | lhs '<'  rhs                                                              { 280 }
  | lhs '>'  rhs                                                              { 281 }
  | lhs '<=' rhs                                                              { 282 }
  | lhs '>=' rhs                                                              { 283 }
  | lhs '&&' rhs                                                              { 284 }
  | lhs '||' rhs                                                              { 285 }
  | lhs '..'        %prec POSTFIXRNG                                          { 286 }
  | lhs '...'       %prec POSTFIXRNG                                          { 287 }
  | lhs '..'  rhs2  %prec INFIXRNG                                            { 288 }
  | lhs '...' rhs2  %prec INFIXRNG                                            { 289 }
  | lhs '<-' rhs                                                              { 290 }
  | lhs '=' rhs                                                               { 291 }
  | lhs '>>=' rhs                                                             { 292 }
  | lhs '<<=' rhs                                                             { 293 }
  | lhs '-=' rhs                                                              { 294 }
  | lhs '+=' rhs                                                              { 295 }
  | lhs '*=' rhs                                                              { 296 }
  | lhs '/=' rhs                                                              { 297 }
  | lhs '^=' rhs                                                              { 298 }
  | lhs '|=' rhs                                                              { 299 }
  | lhs '&=' rhs                                                              { 300 }
  | lhs '%=' rhs                                                              { 301 } 

postfix_blockexpr(lhs) :: { Int }
  : lhs '?'                                                                   { 302 }
  | lhs '.' ident       %prec FIELD                                           { 303 }
  | lhs '.' ident '(' sep_byT(expr,',') ')'                                   { 304 }
  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'      { 305 }
  | lhs '.' int                                                               { 306 }

expr :: { Int }
  : gen_expression(expr,expr,expr)                                            { 307 }
  | paren_expr                                                                { 308 }
  | struct_expr                                                               { 309 }
  | block_expr                                                                { 310 }
  | lambda_expr_block                                                         { 311 }

nostruct_expr :: { Int }
  : gen_expression(nostruct_expr,nostruct_expr,nonstructblock_expr)           { 312 }
  | paren_expr                                                                { 313 }
  | block_expr                                                                { 314 }

nonstructblock_expr :: { Int }
  : gen_expression(nonstructblock_expr,nostruct_expr,nonstructblock_expr)     { 315 }
  | paren_expr                                                                { 316 }
  | block_like_expr                                                           { 317 }
  | unsafe inner_attrs_block                                                  { 318 }

nonblock_expr :: { Int }
  : gen_expression(nonblock_expr,expr,expr)                                   { 319 }
  | paren_expr                                                                { 320 }
  | struct_expr                                                               { 321 }
  | lambda_expr_block                                                         { 322 }

blockpostfix_expr :: { Int }
  : postfix_blockexpr(block_like_expr)                                        { 323 }
  | postfix_blockexpr(vis_safety_block)                                       { 324 }
  | left_gen_expression(blockpostfix_expr,expr,expr)                          { 325 } 

lit_expr :: { Int }
  : lit                                                                       { 326 }

block_expr :: { Int }
  : block_like_expr                                                           { 327 }
  | inner_attrs_block                                                         { 328 }
  | unsafe inner_attrs_block                                                  { 329 } 


block_like_expr :: { Int }
  : if_expr                                                                   { 330 }
  |              loop                            inner_attrs_block            { 331 }
  | lifetime ':' loop                            inner_attrs_block            { 332 }
  |              for pat in nostruct_expr        inner_attrs_block            { 333 }
  | lifetime ':' for pat in nostruct_expr        inner_attrs_block            { 334 }
  |              while             nostruct_expr inner_attrs_block            { 335 }
  | lifetime ':' while             nostruct_expr inner_attrs_block            { 336 }
  |              while let pat '=' nostruct_expr inner_attrs_block            { 337 }
  | lifetime ':' while let pat '=' nostruct_expr inner_attrs_block            { 338 }
  | match nostruct_expr '{'                  '}'                              { 339 }
  | match nostruct_expr '{' inner_attrs      '}'                              { 340 }
  | match nostruct_expr '{'             arms '}'                              { 341 }
  | match nostruct_expr '{' inner_attrs arms '}'                              { 342 }
  | expr_path '!' '{' token_stream '}'                                        { 343 }
  | do catch inner_attrs_block                                                { 344 }

if_expr :: { Int }
  : if             nostruct_expr block else_expr                              { 345 }
  | if let pat '=' nostruct_expr block else_expr                              { 346 }

else_expr :: { Int }
  : else block                                                                { 347 }
  | else if_expr                                                              { 348 }
  | {- empty -}                                                               { 349 }

arms :: { Int }
  : ntArm                                                                     { 350 }
  | ntArm arms                                                                { 351 }
  | many(outer_attribute) sep_by1(pat,'|') arm_guard '=>' expr_arms           { 352 }

arm_guard :: { Int }
  : {- empty -}                                                               { 353 }
  | if expr                                                                   { 354 }

comma_arms :: { Int }
  : {- empty -}                                                               { 355 }
  | ','                                                                       { 356 }
  | ',' arms                                                                  { 357 }

expr_arms :: { Int }
  : nonblock_expr                           comma_arms                        { 358 }
  | blockpostfix_expr                       comma_arms                        { 359 }
  | vis_safety_block                        comma_arms                        { 360 }
  | vis_safety_block                              arms                        { 361 }
  | block_like_expr                         comma_arms                        { 362 }
  | block_like_expr                               arms                        { 363 }

paren_expr :: { Int }
  : '(' ')'                                                                   { 364 }
  | '(' inner_attrs ')'                                                       { 365 }
  | '('             expr ')'                                                  { 366 }
  | '(' inner_attrs expr ')'                                                  { 367 }
  | '('             expr ',' ')'                                              { 368 }
  | '(' inner_attrs expr ',' ')'                                              { 369 }
  | '('             expr ',' sep_by1T(expr,',') ')'                           { 370 }
  | '(' inner_attrs expr ',' sep_by1T(expr,',') ')'                           { 371 }

lambda_expr_block :: { Int }
  : move lambda_args '->' ty_no_plus block                                    { 372 }
  |      lambda_args '->' ty_no_plus block                                    { 373 }

lambda_args :: { Int }
  : '||'                                                                      { 374 }
  | '|' sep_byT(lambda_arg,',') '|'                                           { 375 }

struct_expr :: { Int }
  : expr_path '{'                                    '..' expr '}'            { 376 }
  | expr_path '{' inner_attrs                        '..' expr '}'            { 377 }
  | expr_path '{'             sep_by1(field,',') ',' '..' expr '}'            { 378 }
  | expr_path '{' inner_attrs sep_by1(field,',') ',' '..' expr '}'            { 379 }
  | expr_path '{'             sep_byT(field,',')               '}'            { 380 }
  | expr_path '{' inner_attrs sep_byT(field,',')               '}'            { 381 }

field :: { Int }
  : ident ':' expr                                                            { 382 }
  | ident                                                                     { 383 }

vis_safety_block :: { Int }
  : pub_or_inherited safety inner_attrs_block                                 { 384 } 

vis_union_nonblock_expr :: { Int }
  : union_expr                                                                { 385 }
  | left_gen_expression(vis_union_nonblock_expr, expr, expr)                  { 386 }

union_expr :: { Int }
  : pub_or_inherited union                                                    { 387 }

stmt :: { Int }
  : ntStmt                                                                    { 388 }
  | many(outer_attribute) let pat ':' ty initializer ';'                      { 389 }
  | many(outer_attribute) let pat        initializer ';'                      { 390 }
  | many(outer_attribute) nonblock_expr ';'                                   { 391 }
  | many(outer_attribute) block_like_expr ';'                                 { 392 }
  | many(outer_attribute) blockpostfix_expr ';'                               { 393 }
  | many(outer_attribute) vis_union_nonblock_expr ';'                         { 394 } 
  | many(outer_attribute) block_like_expr    %prec NOSEMI                     { 395 }
  | many(outer_attribute) vis_safety_block ';'                                { 396 }
  | many(outer_attribute) vis_safety_block   %prec NOSEMI                     { 397 }
  | gen_item(pub_or_inherited)                                                { 398 }
  | many(outer_attribute) expr_path '!' ident '[' token_stream ']' ';'        { 399 }
  | many(outer_attribute) expr_path '!' ident '(' token_stream ')' ';'        { 400 }
  | many(outer_attribute) expr_path '!' ident '{' token_stream '}'            { 401 }

pub_or_inherited :: { Int }
  : pub                                          %prec VIS                    { 402 }
  | {- empty -}                                  %prec VIS                    { 403 }

stmtOrSemi :: { Int }
  : ';'                                                                       { 404 }
  | stmt                                                                      { 405 }

stmts_possibly_no_semi :: { Int }
  : stmtOrSemi stmts_possibly_no_semi                                         { 406 }
  | stmtOrSemi                                                                { 407 }
  | many(outer_attribute) nonblock_expr                                       { 408 }
  | many(outer_attribute) blockpostfix_expr                                   { 409 }

initializer :: { Int }
  : '=' expr                                                                  { 410 }
  | {- empty -}                                                               { 411 }

block :: { Int }
  : ntBlock                                                                   { 412 }
  | '{' '}'                                                                   { 413 }
  | '{' stmts_possibly_no_semi '}'                                            { 414 }

inner_attrs_block :: { Int }
  : block                                                                     { 415 }
  | '{' inner_attrs '}'                                                       { 416 }
  | '{' inner_attrs stmts_possibly_no_semi '}'                                { 417 }

gen_item(vis) :: { Int }
  : many(outer_attribute) vis static     ident ':' ty '=' expr ';'                                                    { 418 }
  | many(outer_attribute) vis static mut ident ':' ty '=' expr ';'                                                    { 419 }
  | many(outer_attribute) vis const ident ':' ty '=' expr ';'                                                         { 420 }
  | many(outer_attribute) vis type ident generics where_clause '=' ty ';'                                             { 421 }
  | many(outer_attribute) vis use view_path ';'                                                                       { 422 } 
  | many(outer_attribute) vis safety extern crate ident ';'                                                           { 423 }
  | many(outer_attribute) vis safety extern crate ident as ident ';'                                                  { 424 }
  | many(outer_attribute) vis const safety  fn ident generics fn_decl(arg_named) where_clause inner_attrs_block       { 425 }
  | many(outer_attribute) vis safety extern abi fn ident generics fn_decl(arg_named) where_clause inner_attrs_block   { 426 }
  | many(outer_attribute) vis safety            fn ident generics fn_decl(arg_named) where_clause inner_attrs_block   { 427 }
  | many(outer_attribute) vis mod ident ';'                                                                           { 428 }
  | many(outer_attribute) vis mod ident '{'             many(mod_item) '}'                                            { 429 }
  | many(outer_attribute) vis mod ident '{' inner_attrs many(mod_item) '}'                                            { 430 }
  | many(outer_attribute) vis safety extern abi '{'             many(foreign_item) '}'                                { 431 }
  | many(outer_attribute) vis safety extern abi '{' inner_attrs many(foreign_item) '}'                                { 432 }
  | many(outer_attribute) vis struct ident generics struct_decl_args                                                  { 433 }
  | many(outer_attribute) vis union ident generics struct_decl_args                                                   { 434 }
  | many(outer_attribute) vis enum ident generics where_clause '{' sep_byT(enum_def,',') '}'                          { 435 }
  | many(outer_attribute) vis safety trait ident generics where_clause '{' many(trait_item) '}'                       { 437 }
  | many(outer_attribute) vis         safety impl generics ty_prim              where_clause '{' impl_items '}'       { 438 }
  | many(outer_attribute) vis default safety impl generics ty_prim              where_clause '{' impl_items '}'       { 439 }
  | many(outer_attribute) vis         safety impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'       { 440 }
  | many(outer_attribute) vis default safety impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'       { 441 }
  | many(outer_attribute) vis         safety impl generics '!' trait_ref for ty where_clause '{' impl_items '}'       { 442 }
  | many(outer_attribute) vis default safety impl generics '!' trait_ref for ty where_clause '{' impl_items '}'       { 443 }
  | many(outer_attribute) vis         safety impl generics     trait_ref for ty where_clause '{' impl_items '}'       { 444 }
  | many(outer_attribute) vis default safety impl generics     trait_ref for ty where_clause '{' impl_items '}'       { 445 }
  | many(outer_attribute) vis safety impl generics             trait_ref for '..'            '{'            '}'       { 446 }

mod_item :: { Int }
  : ntItem                                                                    { 447 }
  | gen_item(vis)                                                             { 448 }
  | many(outer_attribute) expr_path '!' ident '[' token_stream ']' ';'        { 449 }
  | many(outer_attribute) expr_path '!'       '[' token_stream ']' ';'        { 450 }
  | many(outer_attribute) expr_path '!' ident '(' token_stream ')' ';'        { 451 }
  | many(outer_attribute) expr_path '!'       '(' token_stream ')' ';'        { 452 }
  | many(outer_attribute) expr_path '!' ident '{' token_stream '}'            { 453 }
  | many(outer_attribute) expr_path '!'       '{' token_stream '}'            { 454 }

foreign_item :: { Int }
  : many(outer_attribute) vis static     ident ':' ty ';'                     { 455 }
  | many(outer_attribute) vis static mut ident ':' ty ';'                     { 456 }
  | many(outer_attribute) vis fn ident generics fn_decl(arg_named) where_clause ';' { 457 }



generics :: { Int }
  : ntGenerics                                                                { 458 }
  | '<' sep_by1(lifetime_def,',') ',' sep_by1T(ty_param,',') gt '>'           { 459 }
  | '<' sep_by1T(lifetime_def,',')                           gt '>'           { 460 }
  | '<'                               sep_by1T(ty_param,',') gt '>'           { 461 }
  | '<'                                                      gt '>'           { 462 }
  | {- empty -}                                                               { 463 }

ty_param :: { Int }
  : many(outer_attribute) ident                                               { 464 }
  | many(outer_attribute) ident ':' sep_by1T(ty_param_bound_mod,'+')          { 465 }
  | many(outer_attribute) ident                                      '=' ty   { 466 }
  | many(outer_attribute) ident ':' sep_by1T(ty_param_bound_mod,'+') '=' ty   { 467 }

struct_decl_args :: { Int }
  : where_clause ';'                                                          { 468 }
  | where_clause '{' sep_byT(struct_decl_field,',') '}'                       { 469 }
  | '(' sep_byT(tuple_decl_field,',') ')' where_clause ';'                    { 470 }

struct_decl_field :: { Int }
  : many(outer_attribute) vis ident ':' ty                                    { 471 }

tuple_decl_field :: { Int }
  : many(outer_attribute) vis ty                                              { 472 }

enum_def :: { Int }
  : many(outer_attribute) ident '{' sep_byT(struct_decl_field,',') '}'        { 473 } 
  | many(outer_attribute) ident '(' sep_byT(tuple_decl_field,',')  ')'        { 474 }
  | many(outer_attribute) ident initializer                                   { 475 } 

where_clause :: { Int }
  : {- empty -}                                                               { 476 }
  | ntWhereClause                                                             { 477 }
  | where sep_by(where_predicate,',')      %prec WHERE                        { 478 }
  | where sep_by1(where_predicate,',') ',' %prec WHERE                        { 479 }

where_predicate :: { Int }
  : lifetime                                                                  { 480 }
  | lifetime ':' sep_by1T(lifetime,'+')                                       { 481 }
  | no_for_ty                                     %prec EQ                    { 482 }
  | no_for_ty '=' ty                                                          { 483 }
  | no_for_ty ':' sep_by1T(ty_param_bound_mod,'+')                            { 484 }
  | for_lts no_for_ty                                                         { 485 }
  | for_lts no_for_ty ':' sep_by1T(ty_param_bound_mod,'+')                    { 486 }

impl_items :: { Int }
  :             many(impl_item)                                               { 487 }
  | inner_attrs many(impl_item)                                               { 488 }

impl_item :: { Int }
  : many(outer_attribute) vis def type ident '=' ty ';'                       { 489 }
  | many(outer_attribute) vis def const ident ':' ty '=' expr ';'             { 490 }
  | many(outer_attribute)     def mod_mac                                     { 491 }

trait_item :: { Int }
  : ntTraitItem                                                               { 494 }
  | many(outer_attribute) const ident ':' ty initializer ';'                  { 495 }
  | many(outer_attribute) mod_mac                                             { 496 }
  | many(outer_attribute) type ident ';'                                      { 497 }
  | many(outer_attribute) type ident '=' ty ';'                               { 498 }
  | many(outer_attribute) type ident ':' sep_by1T(ty_param_bound_mod,'+') ';' { 499 }

safety :: { Int }
  : {- empty -}                                                               { 503 }
  | unsafe                                                                    { 504 }

ext_abi :: { Int }
  : {- empty -}                                                               { 505 }
  | extern abi                                                                { 506 }

vis :: { Int }
  : {- empty -}   %prec VIS                                                   { 507 }
  | pub           %prec VIS                                                   { 508 }
  | pub '(' crate ')'                                                         { 509 }
  | pub '(' in mod_path ')'                                                   { 510 }
  | pub '(' super ')'                                                         { 511 }
  | pub '(' self ')'                                                          { 512 }

def :: { Int }
  : {- empty -}  %prec DEF                                                    { 513 } 
  | default                                                                   { 514 } 

view_path :: { Int }
  : '::' sep_by1(self_or_ident,'::')                                          { 515 } 
  | '::' sep_by1(self_or_ident,'::') as ident                                 { 516 }
  | '::'                                  '*'                                 { 517 }
  | '::' sep_by1(self_or_ident,'::') '::' '*'                                 { 518 }
  | '::' sep_by1(self_or_ident,'::') '::' '{' sep_byT(plist,',') '}'          { 519 }
  | '::'                                  '{' sep_byT(plist,',') '}'          { 520 }
  |      sep_by1(self_or_ident,'::')                                          { 521 }
  |      sep_by1(self_or_ident,'::') as ident                                 { 522 }
  |                                       '*'                                 { 523 }
  |      sep_by1(self_or_ident,'::') '::' '*'                                 { 524 }
  |      sep_by1(self_or_ident,'::') '::' '{' sep_byT(plist,',') '}'          { 525 }
  |                                       '{' sep_byT(plist,',') '}'          { 526 }

self_or_ident :: { Int }
  : ident                                                                     { 527 }
  | self                                                                      { 528 }
  | Self                                                                      { 529 }
  | super                                                                     { 530 }

plist :: { Int }
  : self_or_ident                                                             { 531 }
  | self_or_ident as ident                                                    { 532 }

expr_mac :: { Int }
  : expr_path '!' '[' token_stream ']'                                        { 533 }
  | expr_path '!' '(' token_stream ')'                                        { 534 }

ty_mac :: { Int }
  : ty_path '!' '[' token_stream ']'                                          { 535 }
  | ty_path '!' '{' token_stream '}'                                          { 536 }
  | ty_path '!' '(' token_stream ')'                                          { 537 }

mod_mac :: { Int }
  : mod_path '!' '[' token_stream ']' ';'                                     { 538 }
  | mod_path '!' '{' token_stream '}'                                         { 539 }
  | mod_path '!' '(' token_stream ')' ';'                                     { 540 }

token_stream :: { Int }
  : {- empty -}                                                               { 541 }
  | some(token_tree)                                                          { 542 }

token_tree :: { Int }
  : ntTT                                                                      { 543 }
  | '(' token_stream ')'                                                      { 544 }
  | '{' token_stream '}'                                                      { 545 }
  | '[' token_stream ']'                                                      { 546 }
  | token                                                                     { 547 }

token :: { Int }
  : '='                                                                       { 548 }
  | '<'                                                                       { 549 }
  | '>'                                                                       { 550 }
  | '!'                                                                       { 551 }
  | '~'                                                                       { 552 }
  | '-'                                                                       { 553 }
  | '/'                                                                       { 554 }
  | '+'                                                                       { 555 }
  | '*'                                                                       { 556 }
  | '%'                                                                       { 557 }
  | '^'                                                                       { 558 }
  | '&'                                                                       { 559 }
  | '|'                                                                       { 560 }
  | '<<='                                                                     { 561 }
  | '>>='                                                                     { 562 }
  | '-='                                                                      { 563 }
  | '&='                                                                      { 564 }
  | '|='                                                                      { 565 }
  | '+='                                                                      { 566 }
  | '*='                                                                      { 567 }
  | '/='                                                                      { 568 }
  | '^='                                                                      { 569 }
  | '%='                                                                      { 571 }
  | '||'                                                                      { 572 }
  | '&&'                                                                      { 573 }
  | '=='                                                                      { 574 }
  | '!='                                                                      { 575 }
  | '<='                                                                      { 576 }
  | '>='                                                                      { 577 }
  | '<<'                                                                      { 578 }
  | '>>'                                                                      { 579 }
  | '@'                                                                       { 580 }
  | '...'                                                                     { 581 }
  | '..'                                                                      { 582 }
  | '.'                                                                       { 583 }
  | ','                                                                       { 584 }
  | ';'                                                                       { 585 }
  | '::'                                                                      { 586 }
  | ':'                                                                       { 587 }
  | '->'                                                                      { 588 }
  | '<-'                                                                      { 589 }
  | '=>'                                                                      { 590 }
  | '#'                                                                       { 591 }
  | '$'                                                                       { 592 }
  | '?'                                                                       { 593 }
  | '#!'                                                                      { 594 }
  | byte                                                                      { 595 }
  | char                                                                      { 596 }
  | int                                                                       { 597 }
  | float                                                                     { 598 }
  | str                                                                       { 599 }
  | byteStr                                                                   { 600 }
  | rawStr                                                                    { 601 }
  | rawByteStr                                                                { 602 }
  | as                                                                        { 603 }
  | box                                                                       { 604 }
  | break                                                                     { 605 }
  | const                                                                     { 606 }
  | continue                                                                  { 607 }
  | crate                                                                     { 608 }
  | else                                                                      { 609 }
  | enum                                                                      { 610 }
  | extern                                                                    { 611 }
  | false                                                                     { 612 }
  | fn                                                                        { 613 }
  | for                                                                       { 614 }
  | if                                                                        { 615 }
  | impl                                                                      { 616 }
  | in                                                                        { 617 }
  | let                                                                       { 618 }
  | loop                                                                      { 619 }
  | match                                                                     { 620 }
  | mod                                                                       { 621 }
  | move                                                                      { 622 }
  | mut                                                                       { 623 }
  | pub                                                                       { 624 }
  | ref                                                                       { 625 }
  | return                                                                    { 626 }
  | Self                                                                      { 627 }
  | self                                                                      { 628 }
  | static                                                                    { 629 }
  | struct                                                                    { 630 }
  | super                                                                     { 631 }
  | trait                                                                     { 632 }
  | true                                                                      { 633 }
  | type                                                                      { 634 }
  | unsafe                                                                    { 635 }
  | use                                                                       { 636 }
  | where                                                                     { 637 }
  | while                                                                     { 638 }
  | abstract                                                                  { 639 }
  | alignof                                                                   { 640 }
  | become                                                                    { 641 }
  | do                                                                        { 642 }
  | final                                                                     { 643 }
  | macro                                                                     { 644 }
  | offsetof                                                                  { 645 }
  | override                                                                  { 646 }
  | priv                                                                      { 647 }
  | proc                                                                      { 648 }
  | pure                                                                      { 649 }
  | sizeof                                                                    { 650 }
  | typeof                                                                    { 651 }
  | unsized                                                                   { 652 }
  | virtual                                                                   { 653 }
  | yield                                                                     { 654 }
  | default                                                                   { 655 }
  | union                                                                     { 656 }
  | catch                                                                     { 657 }
  | outerDoc                                                                  { 658 }
  | innerDoc                                                                  { 659 }
  | IDENT                                                                     { 660 }
  | '_'                                                                       { 661 }
  | LIFETIME                                                                  { 662 }

export_attribute :: { Int }
  : inner_attribute                                                           { 663 }
  | outer_attribute                                                           { 664 }

export_block :: { Int }
  : ntBlock                                                                   { 665 }
  | safety '{' '}'                                                            { 666 }
  | safety '{' stmts_possibly_no_semi '}'                                     { 667 } 

export_ty :: { Int }
  : ty                                                                        { 668 }
  | impl_ty                                                                   { 669 }


{

type P a = String -> Either String (a, String)

bindP :: P a -> (a -> P b) -> P b
bindP p f s = case p s of
                Left m -> Left m
                Right (x,s') -> f x s'

returnP :: a -> P a
returnP x s = Right (x,s)

parseError :: Show b => b -> P a
parseError b _ = Left ("Syntax error: the symbol `" ++ show b ++ "' does not fit here")


data Token
  = Equal
  | Less
  | Greater
  | Ampersand
  | Pipe
  | Exclamation
  | Tilde
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | GreaterEqual
  | GreaterGreaterEqual
  | AmpersandAmpersand
  | PipePipe
  | LessLess
  | GreaterGreater
  | EqualEqual
  | NotEqual
  | LessEqual
  | LessLessEqual
  | MinusEqual
  | AmpersandEqual
  | PipeEqual
  | PlusEqual
  | StarEqual
  | SlashEqual
  | CaretEqual
  | PercentEqual
  | At
  | Dot
  | DotDot
  | DotDotDot
  | Comma
  | Semicolon
  | Colon
  | ModSep
  | RArrow
  | LArrow
  | FatArrow
  | Pound
  | Dollar
  | Question
  | OpenParen
  | OpenBracket
  | OpenBrace
  | CloseParen
  | CloseBracket
  | CloseBrace
  | IdentTok String
  | Underscore
  | LifetimeTok String
  | Space
  | InnerDoc
  | OuterDoc
  | Shebang
  | Eof
  | ByteTok String
  | CharTok String
  | IntegerTok String
  | FloatTok String
  | StrTok String
  | StrRawTok String
  | ByteStrTok String
  | ByteStrRawTok String
  | Interpolated Int
  deriving Show 


-- This is an intentionally simplified tokenizer
lexNonSpace :: P Token
lexNonSpace "" = Right (Eof, "")
lexNonSpace ('.':cs) = Right (Dot, cs)
lexNonSpace ('+':cs) = Right (Plus, cs)
lexNonSpace (';':cs) = Right (Semicolon, cs)
lexNonSpace (',':cs) = Right (Comma, cs)
lexNonSpace ('=':cs) = Right (Equal, cs)
lexNonSpace ('{':cs) = Right (OpenBrace, cs)
lexNonSpace ('}':cs) = Right (CloseBrace, cs)
lexNonSpace ('(':cs) = Right (OpenParen, cs)
lexNonSpace (')':cs) = Right (CloseParen, cs)
lexNonSpace (c:cs)
  | isSpace c = lexNonSpace cs
  | isNumber c = let (tok,cs') = span isNumber (c:cs) in Right (IntegerTok tok, cs')
  | isAlpha c = let (tok,cs') = span isAlphaNum (c:cs) in Right (IdentTok tok, cs')
  | otherwise = Left ("Unexpected character: `" ++ [c] ++ "'")


main = case parseStmt "union.1 + 2;" of
         Right (394, "") -> pure ()
         _ -> exitWith (ExitFailure 1)
}
