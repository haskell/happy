module Happy.Backend.LALR.SyntaxLib (
    DocExp,
    varE,
    intE,
    appE,
    tupE,
    listE,
    sigD,
    varBind,
    -- DocStmt,
    DocDec,
    renderDocDecs
  ) where

import qualified Text.PrettyPrint as PP

newtype Prec = Prec Int
  deriving (Eq, Ord, Show, Num, Bounded)

atomPrec, appPrec, noPrec :: Prec
atomPrec = maxBound
appPrec = 10
noPrec = (-1)

type StringBuilder = String -> String

fromTextDetails :: PP.TextDetails -> StringBuilder
fromTextDetails td =
  case td of
    PP.Chr c -> (c:)
    PP.Str str -> (str++)
    PP.PStr str -> (str++)

renderDocDecs :: [[DocDec]] -> StringBuilder
renderDocDecs dss =
  PP.fullRender PP.PageMode 80 1.5 (\td s -> fromTextDetails td . s) id d
  where
    d = PP.vcat (map renderGroup dss)
    renderGroup ds = PP.vcat [ d1 | DocDec d1 <- ds ] PP.$$ PP.text ""

newtype DocExp = DocExp (Prec -> PP.Doc)

-- newtype DocStmt = DocStmt Doc

newtype DocDec = DocDec PP.Doc

varE :: String -> DocExp
varE str = DocExp (\_ -> PP.text str)

intE :: Int -> DocExp
intE n = DocExp (\_ -> parensIf (n < 0) (PP.int n))

appE :: DocExp -> DocExp -> DocExp
appE (DocExp e1) (DocExp e2) =
  DocExp $ \p -> parensIf (p > appPrec) $
    PP.sep [e1 appPrec, e2 atomPrec]

tupE :: [DocExp] -> DocExp
tupE ds =
  DocExp $ \_ ->
    PP.parens $ PP.sep $ PP.punctuate PP.comma $
      [d noPrec | DocExp d <- ds]

listE :: [DocExp] -> DocExp
listE ds =
  DocExp $ \_ ->
    PP.brackets $ PP.sep $ PP.punctuate PP.comma $
      [d noPrec | DocExp d <- ds]

sigD :: String -> DocExp -> DocDec
sigD lhs (DocExp rhs) =
  DocDec $
    PP.hang (PP.text lhs PP.<+> PP.text "::") 2 (rhs noPrec)

varBind :: String -> DocExp -> DocDec
varBind lhs (DocExp rhs) =
  DocDec $
    PP.hang (PP.text lhs PP.<+> PP.text "=") 2 (rhs noPrec)

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True = PP.parens
parensIf False = id
