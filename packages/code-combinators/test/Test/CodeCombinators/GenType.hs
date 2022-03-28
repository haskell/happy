module Test.CodeCombinators.GenType where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import qualified Happy.Backend.CodeCombinators.Syntax as SnGen
import qualified Happy.Backend.CodeCombinators.Abstract as AbsGen
import Happy.Backend.CodeCombinators
import Test.CodeCombinators.Common
import Data.List

genIntT :: MonadGen m => m TH.Type
genIntT = return $ intT

genConT :: MonadGen m => m TH.Type
genConT = do
  conName <- genClassName
  return $ conT conName

genVarT :: MonadGen m => m TH.Type
genVarT = do
  varName <- genFunName
  return $ varT varName

genAppT :: MonadGen m => m TH.Type
genAppT = do
  t1 <- genType
  t2 <- genType
  return $ appT t1 t2

genAppManyArgsT :: MonadGen m => m TH.Type
genAppManyArgsT = do
  t <- genType
  ts <- Gen.list (Range.linear 0 5) genType
  return $ appManyArgsT t ts

genType :: MonadGen m => m TH.Type
genType =
  Gen.recursive Gen.choice
    [
        genIntT
      , genConT
      , genVarT
    ]
    [
        genAppT
      , genAppManyArgsT
    ]

typeToString :: TH.Type -> String
typeToString e = SnGen.renderT (typeToDocType e) ""

typeToDocType :: TH.Type -> SnGen.DocType
typeToDocType (TH.ConT nm) =
  SnGen.conT $ SnGen.mkName $ fullName nm

typeToDocType (TH.VarT nm) =
  SnGen.varT $ SnGen.mkName $ fullName nm

typeToDocType (TH.AppT e1 e2) =
  SnGen.appT (typeToDocType e1) (typeToDocType e2)

typeToDocType _ = error "invalid type"


deleteParensT :: TH.Type -> TH.Type
deleteParensT (TH.ParensT t) =
  deleteParensT t

deleteParensT (TH.ConT nm) =
  TH.ConT nm

deleteParensT (TH.VarT nm) =
  TH.VarT nm

deleteParensT (TH.AppT t1 t2) =
 TH.AppT (deleteParensT t1) (deleteParensT t2)

deleteParensT t = error $ "invalid type" ++ show t
