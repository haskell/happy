module Happy.Backend.CodeCombinators where

import qualified Language.Haskell.TH as TH
import Control.Monad.State
import qualified Data.Map as Map
import Data.Kind (Type)
import Data.Foldable
import Data.String

class (IsString (NameT e), Monad (NewNameM e)) => CodeGen e where
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

  sigD :: NameT e -> TypeT e -> DecT e
  funD :: NameT e -> [ClauseT e] -> DecT e


trueE :: CodeGen e => e
trueE = conE "Prelude.True"

falseE :: CodeGen e => e
falseE = conE "Prelude.False"

trueP :: CodeGen e => PatT e
trueP = conP "Prelude.True" []

falseP :: CodeGen e => PatT e
falseP = conP "Prelude.False" []

mulE :: CodeGen e => e
mulE = varE $ mkOpName "Prelude.*"

addE :: CodeGen e => e
addE = varE $ mkOpName "Prelude.+"

subE :: CodeGen e => e
subE = varE $ mkOpName "Prelude.-"

intT :: CodeGen e => TypeT e
intT = conT "Prelude.Int"

appManyArgsE :: CodeGen e => e -> [e] -> e
appManyArgsE fun args = foldl' appE fun args

appManyArgsT :: CodeGen e => TypeT e -> [TypeT e] -> TypeT e
appManyArgsT fun args = foldl' appT fun args

emptyListE :: CodeGen e => e
emptyListE = conE "[]"

emptyListP :: CodeGen e => PatT e
emptyListP = conP "[]" []

fullFunD :: CodeGen e => NameT e -> TypeT e -> [ClauseT e] -> [DecT e]
fullFunD name type_ clauses =
  [
      sigD name type_
    , funD name clauses
  ]

-- this monad keeps map from String names representation to Name
type NameContext e r = StateT (Map.Map String (NameT e)) (NewNameM e) r

-- returns the name if it already exists
-- otherwise function creates a new name, puts it in the map, and returns that name
getName ::  CodeGen e => String -> NameContext e (NameT e)
getName str_name = do
  maybe_name <- gets (Map.lookup str_name)
  case maybe_name of
    Just name ->
      return name
    Nothing -> do
      newName_ <- lift $ newName str_name
      modify $ \treeMap -> Map.insert str_name newName_ treeMap
      return newName_
