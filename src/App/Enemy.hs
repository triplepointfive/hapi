-----------------------------------------------------------------------------
--
-- Module      :  App.Enemy
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

module App.Enemy (
    Enemy (..)
  , newEnemy
) where

import App.Walker
import App.Direction
import App.Cell
import App.Matrix

data Enemy =
    Enemy { enemyPosY  :: !Int
          , enemyPosX  :: !Int
          , enemyDir   :: !Direction
          , enemyCells :: ![[Cell]]
          } deriving (Show, Read)

instance Walker Enemy where
    walkerPosX  = enemyPosX
    walkerPosY  = enemyPosY
    walkerCells = enemyCells
    walkerSetPos w (y, x) = w { enemyPosY = y, enemyPosX = x }
    walkerSetCells w cells = w { enemyCells = cells }
    walkerSetDir w dir = w { enemyDir = dir }
    walkerPos a = (walkerPosY a, walkerPosX a)

newEnemy :: (Int, Int) -> Matrix Char -> Direction -> Enemy
newEnemy (pY, pX) grid dir = Enemy pY pX dir $ matrixIterate grid makeCell
