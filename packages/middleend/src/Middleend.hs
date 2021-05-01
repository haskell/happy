module Middleend(
    mkFirst, genLR0Items, genLookaheads, genLR1States, genActionTable, genGotoTable, countConflicts,
    Lr0State, Lr1State, LookaheadInfo, ActionTable, GotoTable)
    where

import First
import LALR
import NameSet
import Grammar
import Data.Set
import Data.Array

type Lr0State = (Set Lr0Item, [(Name, Int)])
type Lr1State = ([Lr1Item], [(Name, Int)])
type LookaheadInfo = Array Int [(Lr0Item, NameSet)]

-- mkFirst :: Grammar -> ([Name]) -> NameSet
-- defined in First.lhs

genLR0Items :: Grammar -> [Lr0State]
genLR0Items g = LALR.genLR0items g (precalcClosure0 g)

genLookaheads :: Grammar -> [Lr0State] -> ([Name] -> NameSet) -> LookaheadInfo
genLookaheads g sets first =
    let (spont, prop) = propLookaheads g sets first in
        calcLookaheads (length sets) spont prop

genLR1States :: LookaheadInfo -> [Lr0State] -> [Lr1State]
genLR1States = mergeLookaheadInfo

-- genActionTable :: Grammar -> ([Name] -> NameSet) -> [Lr1State] -> ActionTable
-- defined in LALR.lhs

-- genGotoTable :: Grammar -> [Lr0State] -> GotoTable
-- defined in LALR.lhs

-- countConflicts :: ActionTable -> (Array Int (Int, Int), (Int, Int))
-- defined in LALR.lhs