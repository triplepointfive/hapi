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
  , dirsDeltas
  , dirTurnLeft
  , dirTurnRight
) where

import App.Stuff

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

dirsDeltas :: [(Direction, (Int, Int))]
dirsDeltas = mapResult directionDelta dirs

dirTurnRight :: Direction -> Direction
dirTurnRight DirUp    = DirRight
dirTurnRight DirRight = DirDown
dirTurnRight DirDown  = DirLeft
dirTurnRight DirLeft  = DirUp

dirTurnLeft :: Direction -> Direction
dirTurnLeft DirUp    = DirLeft
dirTurnLeft DirLeft  = DirDown
dirTurnLeft DirDown  = DirRight
dirTurnLeft DirRight = DirUp
