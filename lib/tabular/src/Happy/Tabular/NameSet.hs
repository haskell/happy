{-# LANGUAGE ScopedTypeVariables #-}

module Happy.Tabular.NameSet (
    -- * Set type
    NameSet (..),
    -- * Construction
    empty,
    singleton,
    fromList,
    -- * Deletion
    delete,
    -- * Query
    member,
    null,
    -- * Combine
    union,
    unions,
    difference,
    (\\),
    -- * Folds
    foldr,
    -- * Conversion
    -- ** List
    toAscList
  ) where

import           Prelude hiding (foldr, null)

import           Data.Coerce
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import           Happy.Grammar

newtype NameSet = MkNameSet IntSet
  deriving (Read, Show, Eq, Ord)

--

empty :: NameSet
empty = coerce IntSet.empty

singleton :: Name -> NameSet
singleton = coerce IntSet.singleton

fromList :: [Name] -> NameSet
fromList = coerce IntSet.fromList

--

delete :: Name -> NameSet -> NameSet
delete = coerce IntSet.delete

--

member :: Name -> NameSet -> Bool
member = coerce IntSet.member

null :: NameSet -> Bool
null = coerce IntSet.null

--

union :: NameSet -> NameSet -> NameSet
union = coerce IntSet.union

unions :: [NameSet] -> NameSet
unions = coerce . IntSet.unions . fmap coerce

difference :: NameSet -> NameSet -> NameSet
difference = coerce IntSet.difference

(\\) :: NameSet -> NameSet -> NameSet
(\\) = coerce (IntSet.\\)

--

foldr :: forall b. (Name -> b -> b) -> b -> NameSet -> b
foldr = coerce (IntSet.foldr :: (Int -> b -> b) -> b -> IntSet -> b)

--

toAscList :: NameSet -> [Name]
toAscList = coerce IntSet.toAscList
