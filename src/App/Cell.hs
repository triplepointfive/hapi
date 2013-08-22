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
  , visitedCell
) where

import App.Direction

data Cell =
    Cell { cellVisited :: !Bool
         , cellVisits  :: !Int
         } deriving (Show, Read, Eq)

emptyCell :: Cell
emptyCell = Cell False 0

nonEmptyCell :: Cell
nonEmptyCell = Cell True $ length dirs

visitedCell :: Int -> Cell
visitedCell = Cell False

cellVisit :: Cell -> Cell
cellVisit cell = cell { cellVisited = (cellVisited cell) || length dirs == visits,
                        cellVisits  = visits }
  where visits = 1 + cellVisits cell
