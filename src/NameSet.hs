module NameSet (
   NameSet, null, member, empty, singleton,
   union, difference, filter, fold,
   fromList, toAscList
) where

import Prelude hiding ( null, filter )
import Data.IntSet

type NameSet = IntSet
