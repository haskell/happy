module Happy.Backend.CodeCombinators.Syntax
  (
    DocExp(..),
    DocName(..),
    DocClause(..),
    DocDec(..),
    DocRange(..),
    CodeGen(..),
    renderDocDecs,
    renderDocDec,
    render
  )
  where

import Happy.Backend.CodeCombinators
import qualified Text.PrettyPrint as PP
import qualified Language.Haskell.TH as TH
import Control.Monad.Identity (Identity)
import Data.Word
import Data.Char (chr, ord)

newtype Prec = Prec Int
  deriving (Eq, Ord, Show, Bounded)

atomPrec, appPrec, noPrec :: Prec
atomPrec = Prec maxBound
appPrec = Prec 10
noPrec = Prec (-1)


newtype DocExp = DocExp (Prec -> PP.Doc)
newtype DocType = DocType (Prec -> PP.Doc)
newtype DocPat = DocPat (Prec -> PP.Doc)

newtype DocName = DocName PP.Doc
  deriving (Eq, Show)
newtype DocClause = DocClause PP.Doc
  deriving (Eq, Show)
newtype DocDec = DocDec PP.Doc
  deriving (Eq, Show)

data DocRange
  = FromR DocExp
  | FromThenR DocExp DocExp
  | FromToR DocExp DocExp
  | FromThenToR DocExp DocExp DocExp

instance CodeGen DocExp where
  type NameT DocExp = DocName
  type RangeT DocExp = DocRange
  type TypeT DocExp = DocType
  type PatT DocExp = DocPat
  type DecT DocExp = DocDec
  type ClauseT DocExp = DocClause
  type NewNameM DocExp = Identity

  mkName :: String -> DocName
  mkName name = DocName $ PP.text name

  mkOpName :: String -> DocName
  mkOpName name = DocName $ PP.parens $ PP.text name

  newName :: String -> Identity DocName
  newName = return . mkName

  negateE :: DocExp -> DocExp
  negateE = appE $ varE $ mkName "GHC.Num.negate"

  intE :: Integral a => a -> DocExp
  intE num
    | num < 0 = negateE $ absE
    | otherwise = absE
    where absE =
            DocExp $ \_ ->
              PP.integer $ abs $ fromIntegral $ num

  stringE :: String -> DocExp
  stringE str = DocExp $ \_ -> PP.doubleQuotes $ PP.text $ escape str
    where escape ('\'':xs) =  '\\' : '\'' : escape xs
          escape ('\"':xs) =  '\\' : '\"' : escape xs
          escape ('\\':xs) =  '\\' : '\\' : escape xs
          escape (x:xs) = x : escape xs
          escape [] = []

  hexCharsE :: [Int] -> DocExp
  hexCharsE ls =
    DocExp $ \_ ->
      (PP.doubleQuotes $ PP.text hexChars) PP.<> PP.text "#"
    where
      -- these functions are taken from ProduceCode.lhs (happy-backend-lalr package)
      hexChars :: String
      hexChars = concatMap hexChar ls

      hexChar :: Int -> String
      hexChar i | i < 0 = hexChar (i + 65536)
      hexChar i =  toHex (i `mod` 256) ++ toHex (i `div` 256)

      toHex :: Int -> String
      toHex i = ['\\','x', hexDig (i `div` 16), hexDig (i `mod` 16)]

      hexDig :: Int -> Char
      hexDig i | i <= 9    = chr (i + ord '0')
               | otherwise = chr (i - 10 + ord 'a')

  conE :: DocName -> DocExp
  conE (DocName name) = DocExp $ \_ -> name

  varE :: DocName -> DocExp
  varE (DocName name) = DocExp $ \_ -> name

  appE :: DocExp -> DocExp -> DocExp
  appE (DocExp e1) (DocExp e2) =
    DocExp $ \p ->
      parensIf (p > appPrec) $
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

  arithSeqE :: DocRange -> DocExp
  arithSeqE (FromToR (DocExp e1) (DocExp e2)) =
    DocExp $ \_ ->
      PP.brackets  $ e1 noPrec PP.<+> PP.text ".." PP.<+> e2 noPrec

  conT :: DocName -> DocType
  conT (DocName name) = DocType $ \_ -> name

  varT :: DocName -> DocType
  varT (DocName name) = DocType $ \_ -> name

  appT :: DocType -> DocType -> DocType
  appT (DocType t1) (DocType t2) =
    DocType $ \p ->
      parensIf (p > appPrec) $
      PP.sep [t1 appPrec, t2 atomPrec]

  litP :: TH.Lit -> DocPat
  litP (TH.CharL c) = DocPat $ \_ -> PP.quotes $ PP.text [c]
  litP (TH.StringL s) = DocPat $ \_ -> PP.doubleQuotes $ PP.text s
  litP (TH.IntegerL n) = DocPat $ \_ -> parensIf (n < 0) $ PP.text $ show n

  varP :: DocName -> DocPat
  varP (DocName name)    = DocPat $ \_ -> name

  tupP :: [DocPat] -> DocPat
  tupP ps =
    DocPat $ \_ ->
      PP.parens $ PP.sep $
      PP.punctuate PP.comma [p noPrec | DocPat p <- ps]

  conP :: DocName -> [DocPat] -> DocPat
  conP (DocName name) ps =
    DocPat $ \p ->
      parensIf (p > appPrec) $
      name PP.<+> PP.sep [pt atomPrec | DocPat pt <- ps]

  wildP :: DocPat
  wildP = DocPat $ \_ -> PP.text "_"

  clause :: [DocPat] -> DocExp -> [DocDec] -> DocClause
  clause ps (DocExp exp) decs =
    DocClause $
      (PP.sep [p noPrec | DocPat p <- ps] PP.<+> PP.text "=" PP.<+> exp noPrec)
      PP.$+$ PP.nest 4 whereSection
    where whereSection =
            case decs of
              [] -> PP.empty
              _  ->
                PP.text "where" PP.$+$
                foldr (PP.$+$) PP.empty [PP.nest 4 dec | DocDec dec <- decs]

  sigD :: DocName -> DocType -> DocDec
  sigD (DocName name) (DocType type_) = DocDec $ name PP.<+> PP.text "::" PP.<+> type_ noPrec

  funD :: DocName -> [DocClause] -> DocDec
  funD (DocName name) cls = DocDec $ foldr1 (PP.$+$) [name PP.<+> cl | DocClause cl <- cls]


fromTextDetails :: PP.TextDetails -> ShowS
fromTextDetails td =
  case td of
    PP.Chr c -> (c:)
    PP.Str str -> (str++)
    PP.PStr str -> (str++)

render :: DocExp -> ShowS
render (DocExp exp) = showString $ PP.render $ exp noPrec


renderDocDec :: DocDec -> ShowS
renderDocDec (DocDec dec) = showString $ PP.render dec

renderDocDecs :: [[DocDec]] -> ShowS
renderDocDecs dss =
  PP.fullRender PP.PageMode 120 1.5 (\td s -> fromTextDetails td . s) id d
  where
    d = PP.vcat (map renderGroup dss)
    renderGroup ds = PP.vcat [ d1 | DocDec d1 <- ds ] PP.$+$ PP.text ""

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True = PP.parens
parensIf False = id
