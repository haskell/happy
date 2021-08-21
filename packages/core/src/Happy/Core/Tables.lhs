Contains datatypes for goto and action tables which are consumed by happy-backend.

> module Happy.Core.Tables (
>    LRAction(..), ActionTable, Goto(..), GotoTable
> ) where

> import Happy.Core.Grammar

> import Data.Array

> data LRAction = LR'Shift Int Priority -- state number and priority
>               | LR'Reduce Int Priority-- rule no and priority
>               | LR'Accept             -- :-)
>               | LR'Fail               -- :-(
>               | LR'MustFail           -- :-(
>               | LR'Multiple [LRAction] LRAction       -- conflict
>       deriving (Eq, Show)

> type ActionTable = Array Int{-state-} (Array Int{-terminal#-} LRAction)

> data Goto = Goto Int | NoGoto
>       deriving(Eq, Show)

> type GotoTable = Array Int{-state-} (Array Int{-nonterminal #-} Goto)
