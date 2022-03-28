module Test.CodeCombinators.GenDec where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import qualified Happy.Backend.CodeCombinators.Syntax as SnGen
import Happy.Backend.CodeCombinators
import Test.CodeCombinators.Common
import Test.CodeCombinators.GenExp
import Test.CodeCombinators.GenPat
import Test.CodeCombinators.GenType

genClause :: MonadGen m => Int -> m TH.Clause
genClause pNumber = do
    ps <- Gen.list (Range.linear pNumber pNumber) genPat
    e <- genExp
    ds <- genDecList
    return $ clause ps e ds

genSigD :: MonadGen m => m TH.Dec
genSigD = do
    nm <- genFunName
    tp <- genType
    return $ sigD nm tp

genFunD :: MonadGen m => m TH.Dec
genFunD = do
    nm <- genFunName
    pn <- Gen.int $ Range.linear 1 5
    cls <- Gen.list (Range.linear 1 20) (genClause pn)
    return $ funD nm cls

genDecList :: MonadGen m => m [TH.Dec]
genDecList =
  Gen.recursive Gen.choice
    [ Gen.list (Range.linear 1 5) genSigD ]
    [ Gen.list (Range.linear 1 5) genFunD ]

decListToString :: [TH.Dec] -> String
decListToString ds = SnGen.renderDocDecs [decToDocDec <$> ds] ""

decToDocDec :: TH.Dec -> SnGen.DocDec
decToDocDec (TH.SigD nm tp) =
    SnGen.sigD (SnGen.mkName $ fullName nm) $ typeToDocType tp

decToDocDec (TH.FunD nm cls) =
  SnGen.funD (SnGen.mkName $ fullName nm)
    (clauseToDocClause <$> cls)

decToDocDec _ = error "invalid dec"

clauseToDocClause :: TH.Clause -> SnGen.DocClause
clauseToDocClause (TH.Clause ps (TH.NormalB e) ds) =
  SnGen.clause (patToDocPat <$> ps) (expToDocExp e) (decToDocDec <$> ds)

clauseToDocClause c = error $ "invalid сlause" ++ show c

deleteParensDecList :: [TH.Dec] -> [TH.Dec]
deleteParensDecList = map deleteParensD

deleteParensD :: TH.Dec -> TH.Dec
deleteParensD (TH.SigD nm tp) =
  TH.SigD nm $ deleteParensT tp

deleteParensD (TH.FunD nm cls) =
  TH.FunD nm $
    deleteParensC <$> cls

deleteParensD d = error $ "invalid dec" ++ show d

deleteParensC :: TH.Clause -> TH.Clause
deleteParensC (TH.Clause ps (TH.NormalB e) ds) =
    TH.Clause (deleteParensP <$> ps) (TH.NormalB $ deleteParensE e) (deleteParensD <$> ds)

deleteParensC c = error $ "invalid сlause" ++ show c
