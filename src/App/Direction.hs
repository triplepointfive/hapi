-----------------------------------------------------------------------------
--
-- Module      :  App.Direction
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

module App.Direction (
    Direction (..)
  , directionDelta
  , dirs
) where

data Direction = DirUp
               | DirDown
               | DirLeft
               | DirRight
               deriving (Show, Read, Eq)

directionDelta :: Direction -> (Int, Int)
directionDelta DirUp    = ( 1,  0)
directionDelta DirDown  = (-1,  0)
directionDelta DirLeft  = ( 0, -1)
directionDelta DirRight = ( 0,  1)

dirs :: [Direction]
dirs = [DirUp, DirDown, DirLeft, DirRight]
