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

import App.Direction
import App.Walker

performLogic :: Walker -> Walker
performLogic walker = walkerTurn walker mapA
