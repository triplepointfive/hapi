-----------------------------------------------------------------------------
--
-- Module      :  Map
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

module App.Map (
    Walker (..)
  , walkerPos
  , performLogic
  , walkerPrintCell
) where

import Data.Maybe

import App.Direction
import App.Cell
import App.Matrix
import App.Walker

performLogic :: Walker -> (Int, Int) -> Walker
performLogic walker sizes =
    if walkerCollision updatedWalker sizes
    then walkerVisitCell updatedWalker
    else walkerChangeDir walker
  where updatedWalker = walkerMove walker $ (directionDelta . walkerDir) walker
