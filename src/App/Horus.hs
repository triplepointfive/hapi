-----------------------------------------------------------------------------
--
-- Module      :  App.Horus
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

module App.Horus
  ( Horus (..)
  , newHorus
) where

import App.Walker
import App.Direction
import App.Cell
import App.Matrix

data Horus =
    Horus { horusPosY  :: !Int
          , horusPosX  :: !Int
          , horusDir   :: !Direction
          , horusCells :: ![[Cell]]
          } deriving (Show, Read)

instance Walker Horus where
    walkerPosX  = horusPosX
    walkerPosY  = horusPosY
    walkerCells = horusCells
    walkerSetPos w (y, x) = w { horusPosY = y, horusPosX = x }
    walkerSetCells w cells = w { horusCells = cells }
    walkerSetDir w dir = w { horusDir = dir }
    walkerPos a = (walkerPosY a, walkerPosX a)

newHorus :: (Int, Int) -> Matrix Char -> Direction -> Horus
newHorus (pY, pX) grid dir = Horus pY pX dir $ matrixIterate grid makeCell

