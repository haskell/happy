module Test.CodeCombinators.Common where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Haskell.TH as TH
import qualified Happy.Backend.CodeCombinators.Abstract()
import Happy.Backend.CodeCombinators

genFunName :: MonadGen m => m TH.Name
genFunName = do
  name_tail <- Gen.list (Range.linear 1 10) Gen.alphaNum
  name_head <- Gen.lower
  return $ mkName $ (name_head : name_tail) ++ "_"

genClassName :: MonadGen m => m TH.Name
genClassName = do
  name_tail <- Gen.list (Range.linear 1 10) Gen.alphaNum
  name_head <- Gen.upper
  return $ mkName $ (name_head : name_tail) ++ "_"

fullName :: TH.Name -> String
fullName nm =
  moduleName ++ TH.nameBase nm
  where moduleName =
          case TH.nameModule nm of
            Just str -> str ++ "."
            Nothing -> ""
