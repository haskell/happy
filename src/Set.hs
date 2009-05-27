module Set (
   Set, null, member, empty, singleton, insert,
   union, difference, filter, fold,
   fromList, toAscList
) where

import Prelude hiding ( null, filter )
import Data.Set

