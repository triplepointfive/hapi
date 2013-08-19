-----------------------------------------------------------------------------
--
-- Module      :  App.Cell
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module App.Cell (
    Cell (..)
  , emptyCell
  , nonEmptyCell
  , cellVisit
) where

import App.Stuff ( itemsEqualOnDiff )
import App.Direction

data Cell =
    Cell { cellVisited :: !Bool
         , cellDirections :: ![Direction]
         } deriving (Show, Read, Eq)

emptyCell :: Cell
emptyCell = Cell False []

nonEmptyCell :: Cell
nonEmptyCell = Cell True []

cellVisit :: Cell -> Direction -> Cell
cellVisit cell@(Cell True _) _ = cell
cellVisit orig dir = orig { cellVisited    = itemsEqualOnDiff dirs directions,
                            cellDirections = directions }
  where directions = dir : cellDirections orig
