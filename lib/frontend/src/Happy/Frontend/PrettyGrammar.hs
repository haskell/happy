{-# LANGUAGE CPP #-}
module Happy.Frontend.PrettyGrammar where

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif
import Happy.Frontend.AbsSyn

render :: Doc -> String
render = maybe "" ($ "")

ppAbsSyn :: AbsSyn String -> Doc
ppAbsSyn (AbsSyn ds rs) = vsep (vcat (map ppDirective ds) : map ppRule rs)

ppDirective :: Directive a -> Doc
ppDirective dir =
  case dir of
    TokenNonassoc xs -> prec "%nonassoc" xs
    TokenRight xs    -> prec "%right" xs
    TokenLeft xs     -> prec "%left" xs
    _                -> empty
  where
  prec x xs = text x <+> hsep (map text xs)

ppRule :: Rule String -> Doc
ppRule (Rule name _ prods _) = text name
                            $$ vcat (zipWith (<+>) starts (map ppProd prods))
  where
  starts = text "  :" : repeat (text "  |")

ppProd :: Prod String -> Doc
ppProd (Prod ts _ _ p) = psDoc <+> ppPrec p
  where
  psDoc   = if null ts then text "{- empty -}" else hsep (map ppTerm ts)

ppPrec :: Prec -> Doc
ppPrec PrecNone   = empty
ppPrec PrecShift  = text "%shift"
ppPrec (PrecId x) = text "%prec" <+> text x

ppTerm :: Term -> Doc
ppTerm (App x ts) = text x <> ppTuple (map ppTerm ts)

ppTuple :: [Doc] -> Doc
ppTuple [] = empty
ppTuple xs = parens (hsep (punctuate comma xs))

--------------------------------------------------------------------------------
-- Pretty printing combinator

type Doc = Maybe ShowS

empty :: Doc
empty = Nothing

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []  = []
punctuate _ [x] = [x]
punctuate sep (x : xs) = (x <> sep) : punctuate sep xs

comma ::  Doc
comma = char ','

char :: Char -> Doc
char x = Just (showChar x)

text :: String -> Doc
text x = if null x then Nothing else Just (showString x)

(<+>) :: Doc -> Doc -> Doc
Nothing <+> y     = y
x <+> Nothing     = x
x <+> y           = x <> char ' ' <> y

(<>) :: Doc -> Doc -> Doc
Nothing <> y = y
x <> Nothing = x
Just x <> Just y = Just (x . y)

($$) :: Doc -> Doc -> Doc
Nothing $$ y = y
x $$ Nothing = x
x $$ y       = x <> char '\n' <> y

hsep :: [Doc] -> Doc
hsep = hcat . punctuate (char ' ')

vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

vsep :: [Doc] -> Doc
vsep = vcat . punctuate (char '\n')

parens :: Doc -> Doc
parens x = char '(' <> x <> char ')'

hcat :: [Doc] -> Doc
hcat = foldr (<>) empty


