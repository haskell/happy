module Happy.Backend.CodeCombinators where

import qualified Language.Haskell.TH as TH

class CodeGen e where
  type NameT e = n | n -> e
  type RangeT e = r | r -> e
  type TypeT e = t | t -> e
  type PatT e = p | p -> e
  type DecT e = d | d -> e
  type ClauseT e = c | c -> e

  mkName :: String -> NameT e
  intE :: Int -> e
  stringE :: String -> e

  conE :: NameT e -> e
  varE :: NameT e -> e
  appE :: e -> e -> e

  tupE :: [e] -> e
  listE :: [e] -> e
  arithSeqE :: RangeT e -> e

  conT :: NameT e -> TypeT e
  varT :: NameT e -> TypeT e
  appT :: TypeT e -> TypeT e -> TypeT e

  litP :: TH.Lit -> PatT e
  varP :: NameT e -> PatT e
  tupP :: [PatT e] -> PatT e
  conP :: NameT e -> [PatT e] -> PatT e
  wildP :: PatT e

  clause :: [PatT e] -> e -> [DecT e] -> ClauseT e

  sigD :: NameT e -> TypeT e  -> DecT e
  funD :: NameT e -> [ClauseT e] -> DecT e



trueE :: CodeGen e => e
trueE = conE $ mkName "Prelude.True"

falseE :: CodeGen e => e
falseE = conE $ mkName "Prelude.False"

trueP :: CodeGen e => PatT e
trueP = conP (mkName "Prelude.True") []

falseP :: CodeGen e => PatT e
falseP = conP (mkName "Prelude.False") []

mulE :: CodeGen e => e
mulE = varE $ mkName "(Prelude.*)"

addE :: CodeGen e => e
addE = varE $ mkName "(Prelude.+)"

subE :: CodeGen e => e
subE = varE $ mkName "(Prelude.-)"

intT :: CodeGen e => TypeT e
intT = conT $ mkName "Prelude.Int"

emptyListE :: CodeGen e => e
emptyListE = conE $ mkName "[]"

emptyListP :: CodeGen e => PatT e
emptyListP = conP (mkName "[]") []
