module Happy.Backend.CodeCombinators where

import qualified Language.Haskell.TH as TH
import Control.Monad.State
import qualified Data.Map as Map
import Data.Kind (Type)

class CodeGen e where
  type NameT e = n | n -> e
  type RangeT e = r | r -> e
  type TypeT e = t | t -> e
  type PatT e = p | p -> e
  type DecT e = d | d -> e
  type ClauseT e = c | c -> e
  type NewNameM e :: Type -> Type

  mkName :: String -> NameT e
  mkOpName :: String -> NameT e
  newName :: String -> NewNameM e (NameT e)

  intE :: Integral a => a -> e
  negateE :: e -> e
  stringE :: String -> e
  hexCharsE :: [Int] -> e

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
mulE = varE $ mkOpName "Prelude.*"

addE :: CodeGen e => e
addE = varE $ mkOpName "Prelude.+"

subE :: CodeGen e => e
subE = varE $ mkOpName "Prelude.-"

intT :: CodeGen e => TypeT e
intT = conT $ mkName "Prelude.Int"

emptyListE :: CodeGen e => e
emptyListE = conE $ mkName "[]"

emptyListP :: CodeGen e => PatT e
emptyListP = conP (mkName "[]") []

type NameContext e r = StateT (Map.Map String (NameT e)) (NewNameM e) r

getName ::  (CodeGen e, Monad (NewNameM e)) => String -> NameContext e (NameT e)
getName str_name = do
  maybe_name <- gets (Map.lookup str_name)
  case maybe_name of
    Just name ->
      return name
    Nothing -> do
      newName_ <- lift $ newName str_name
      modify $ \treeMap -> Map.insert str_name newName_ treeMap
      return newName_
