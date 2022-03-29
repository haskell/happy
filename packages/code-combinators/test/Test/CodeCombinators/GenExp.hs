module Test.CodeCombinators.GenExp where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import qualified Happy.Backend.CodeCombinators.Syntax as SnGen
import qualified Happy.Backend.CodeCombinators.Abstract as AbsGen
import Happy.Backend.CodeCombinators
import Test.CodeCombinators.Common
import Data.List
import Data.Maybe

genIntE :: MonadGen m => m TH.Exp
genIntE = do
  x <- Gen.int $ Range.linear minBound maxBound
  return $ intE x

genStringE :: MonadGen m => m TH.Exp
genStringE = do
  str <- Gen.list (Range.linear 0 20) Gen.latin1
  return $ stringE $ delete '\n' str

genConE :: MonadGen m => m TH.Exp
genConE = do
  conName <- genClassName
  return $ TH.ConE conName

genVarE :: MonadGen m => m TH.Exp
genVarE = do
  varName <- genFunName
  return $ TH.VarE varName

genAppE :: MonadGen m => m TH.Exp
genAppE = do
  e1 <- genExp
  e2 <- genExp
  return $ appE e1 e2

genAppManyArgsE :: MonadGen m => m TH.Exp
genAppManyArgsE = do
  e <- genExp
  es <- Gen.list (Range.linear 0 5) genExp
  return $ appManyArgsE e es

genTupE :: MonadGen m => m TH.Exp
genTupE = do
  es <- Gen.list (Range.linear 2 20) genExp
  return $ tupE es

genListE :: MonadGen m => m TH.Exp
genListE = do
  es <- Gen.list (Range.linear 0 20) genExp
  return $ listE es

genArithSeqE :: MonadGen m => m TH.Exp
genArithSeqE = do
  e1 <- genExp
  e2 <- genExp
  return $ TH.ArithSeqE $ TH.FromToR e1 e2

genExp :: MonadGen m => m TH.Exp
genExp =
  Gen.recursive Gen.choice
    [
        genIntE
      , genStringE
      , genConE
      , genVarE
    ]
    [
        genAppE
      , genTupE
      , genListE
      , genArithSeqE
      , genAppManyArgsE
    ]

expToString :: TH.Exp -> String
expToString e = SnGen.renderE (expToDocExp e) ""

expToDocExp :: TH.Exp -> SnGen.DocExp
expToDocExp (TH.LitE l) =
  case l of
    TH.StringL str -> SnGen.stringE str
    TH.IntegerL num -> SnGen.intE $ fromIntegral num
    _ -> error "invalid literal"

expToDocExp (TH.ConE nm) =
  SnGen.conE $ SnGen.mkName $ fullName nm

expToDocExp (TH.VarE nm) =
  SnGen.varE $ SnGen.mkName $ fullName nm

expToDocExp (TH.AppE e1 e2) =
  SnGen.appE (expToDocExp e1) (expToDocExp e2)

expToDocExp (TH.ListE es) =
  SnGen.listE $ map expToDocExp es

expToDocExp (TH.TupE es) =
  SnGen.tupE $ map (\(Just e) -> expToDocExp e) es

expToDocExp (TH.ArithSeqE range) =
  case range of
    TH.FromToR e1 e2 ->
      SnGen.arithSeqE $
        SnGen.FromToR (expToDocExp e1) (expToDocExp e2)
    _ ->
       error "invalid range"

expToDocExp _ = error "invalid exp"


deleteParensE :: TH.Exp -> TH.Exp
deleteParensE (TH.ParensE e) =
  deleteParensE e

deleteParensE (TH.LitE l) =
  TH.LitE l

deleteParensE (TH.ConE nm) =
  TH.ConE nm

deleteParensE (TH.VarE nm) =
  TH.VarE nm

deleteParensE (TH.AppE e1 e2) =
 TH.AppE (deleteParensE e1) (deleteParensE e2)

deleteParensE (TH.ListE es) =
  TH.ListE $ map deleteParensE es

deleteParensE (TH.TupE es) =
  TH.TupE $ map (\(Just e) -> Just $ deleteParensE e) es

deleteParensE (TH.ArithSeqE range) =
  case range of
    TH.FromToR e1 e2 ->
      TH.ArithSeqE $ TH.FromToR (deleteParensE e1) (deleteParensE e2)
    _ ->
      error "invalid range"

deleteParensE e = error $ "invalid exp" ++ show e
