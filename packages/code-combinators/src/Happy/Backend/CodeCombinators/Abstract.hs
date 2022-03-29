module Happy.Backend.CodeCombinators.Abstract where

import Happy.Backend.CodeCombinators
import qualified Language.Haskell.TH as TH
import Data.Word
import Data.String

instance CodeGen TH.Exp where
  type NameT TH.Exp = TH.Name
  type RangeT TH.Exp  = TH.Range
  type TypeT TH.Exp = TH.Type
  type PatT TH.Exp = TH.Pat
  type DecT TH.Exp = TH.Dec
  type ClauseT TH.Exp = TH.Clause
  type NewNameM TH.Exp = TH.Q

  mkName :: String -> TH.Name
  mkName = TH.mkName

  mkOpName :: String -> TH.Name
  mkOpName = TH.mkName

  newName :: String -> TH.Q TH.Name
  newName = TH.newName

  negateE :: TH.Exp -> TH.Exp
  negateE = TH.AppE (TH.VarE $ mkName "GHC.Num.negate")

  intE :: Int -> TH.Exp
  intE num
    | num < 0 = negateE absE
    | otherwise = absE
    where absE =
            TH.LitE $ TH.IntegerL $
            abs $ fromIntegral num

  stringE :: String -> TH.Exp
  stringE str = TH.LitE $ TH.StringL str

  hexCharsE :: [Int] -> TH.Exp
  hexCharsE ls =
    TH.LitE $ TH.StringPrimL hexChars
    where
      -- these functions are analogues of the functions from ProduceCode.lhs (happy-backend-lalr package)
      hexChars :: [Word8]
      hexChars = concatMap hexChar ls

      hexChar :: Int -> [Word8]
      hexChar i | i < 0 = hexChar (i + 65536)
      hexChar i =  toHex (i `mod` 256) ++ toHex (i `div` 256)

      toHex :: Int -> [Word8]
      toHex i = [hexDig (i `div` 16), hexDig (i `mod` 16)]

      hexDig :: Int -> Word8
      hexDig = fromIntegral

  conE :: TH.Name  -> TH.Exp
  conE = TH.ConE

  varE :: TH.Name -> TH.Exp
  varE = TH.VarE

  appE :: TH.Exp -> TH.Exp -> TH.Exp
  appE = TH.AppE


  tupE :: [TH.Exp] -> TH.Exp
  tupE es = TH.TupE $ map Just es

  listE :: [TH.Exp] -> TH.Exp
  listE = TH.ListE

  arithSeqE :: TH.Range -> TH.Exp
  arithSeqE = TH.ArithSeqE

  conT :: TH.Name -> TH.Type
  conT = TH.ConT

  varT :: TH.Name -> TH.Type
  varT = TH.VarT

  appT :: TH.Type -> TH.Type -> TH.Type
  appT = TH.AppT

  litP :: TH.Lit -> TH.Pat
  litP = TH.LitP

  varP :: TH.Name -> TH.Pat
  varP = TH.VarP

  tupP :: [TH.Pat] -> TH.Pat
  tupP = TH.TupP

  conP :: TH.Name -> [TH.Pat] -> TH.Pat
  conP = TH.ConP

  wildP :: TH.Pat
  wildP = TH.WildP

  clause :: [TH.Pat] -> TH.Exp -> [TH.Dec] -> TH.Clause
  clause ps e decs = TH.Clause ps (TH.NormalB e) decs

  sigD :: TH.Name -> TH.Type -> TH.Dec
  sigD = TH.SigD

  funD :: TH.Name -> [TH.Clause] -> TH.Dec
  funD = TH.FunD

  noInlinePragmaD :: TH.Name -> TH.Dec
  noInlinePragmaD name =
    TH.PragmaD $
      TH.InlineP name TH.NoInline TH.FunLike TH.AllPhases

instance IsString TH.Name where
  fromString = mkName
