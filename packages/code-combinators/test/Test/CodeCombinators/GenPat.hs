module Test.CodeCombinators.GenPat where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import qualified Happy.Backend.CodeCombinators.Syntax as SnGen
import qualified Happy.Backend.CodeCombinators.Abstract as AbsGen
import Happy.Backend.CodeCombinators
import Test.CodeCombinators.Common
import Data.List


genLitP :: MonadGen m => m TH.Pat
genLitP = do
  x <- Gen.choice $
         [
            do
            c <- Gen.latin1
            return $ litP $ TH.CharL c
          , do
            s <- Gen.list (Range.linear 0 20) Gen.latin1
            return $ litP $ TH.StringL $ delete '\n' s
          , do
            x <- Gen.int $ Range.linear minBound maxBound
            return $ litP $ TH.IntegerL $ fromIntegral x
         ]
  return x

genBoolP :: MonadGen m => m TH.Pat
genBoolP = Gen.element [trueP, falseP]

genVarP :: MonadGen m => m TH.Pat
genVarP = do
  varName <- genFunName
  return $ varP varName

genWildP :: MonadGen m => m TH.Pat
genWildP = return wildP

genConP :: MonadGen m => m TH.Pat
genConP = do
  conName <- genClassName
  ps <- Gen.list (Range.linear 2 20) genPat
  return $ conP conName ps

genTupP :: MonadGen m => m TH.Pat
genTupP = do
  ps <- Gen.list (Range.linear 2 20) genPat
  return $ tupP ps

genPat :: MonadGen m => m TH.Pat
genPat =
  Gen.recursive Gen.choice
    [
        genLitP
      , genBoolP
      , genVarP
      , genWildP
    ]
    [
        genConP
      , genTupP
    ]

patToString :: TH.Pat -> String
patToString p = SnGen.renderP (patToDocPat p) ""

patToDocPat :: TH.Pat -> SnGen.DocPat
patToDocPat (TH.LitP p) = SnGen.litP p

patToDocPat (TH.ConP nm ps) =
  SnGen.conP
    (SnGen.mkName $ fullName nm)
    (map patToDocPat ps)

patToDocPat (TH.VarP nm) =
  SnGen.varP $ SnGen.mkName $ fullName nm

patToDocPat (TH.TupP ps) =
  SnGen.tupP $ map patToDocPat ps

patToDocPat TH.WildP =
  SnGen.wildP

patToDocPat _ = error "invalid pat"


deleteParensP :: TH.Pat -> TH.Pat
deleteParensP (TH.ParensP p) = deleteParensP p

deleteParensP (TH.LitP p) = TH.LitP p

deleteParensP (TH.ConP nm ps) =
  TH.ConP nm $ map deleteParensP ps

deleteParensP (TH.VarP nm) =
  TH.VarP nm

deleteParensP (TH.TupP ps) =
  TH.TupP $ map deleteParensP ps

deleteParensP TH.WildP =
  TH.WildP

deleteParensP p = error $ "invalid exp" ++ show p
