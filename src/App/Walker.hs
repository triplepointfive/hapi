-----------------------------------------------------------------------------
--
-- Module      :  App.Walker
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

module App.Walker (
    Walker (..)
  , walkerCollision
  , walkerVisitCell
  , walkerChangeDir
  , walkerPos
  , walkerMove
  , walkerPrintCell
  , newWalker
  , mapA
) where


import Data.Maybe

import App.Cell
import App.Direction
import App.Matrix

mapA :: [[Char]]
mapA = ["################################"
      , "#                              #"
      , "# ### ######################## #"
      , "# # ###       #   #          # #"
      , "# #     ##### ### # ######## # #"
      , "# ### ### # #  #  # #      # # #"
      , "#   # # # #   ###   # # ## # # #"
      , "# # ### # ## ######## # ##   # #"
      , "# # ### #  # #   #    #    ### #"
      , "# # # # ## # #   # ####### # # #"
      , "# #        # #   #     #     # #"
      , "# ######## # ## ###### # ### # #"
      , "# # #      #  # #   #  # # # # #"
      , "# # #### #### # # # # ## # # # #"
      , "#   #       # # # # #  #     # #"
      , "# ### ## #### # # #### ##### # #"
      , "# # ###  #  # # # #        # # #"
      , "# # ### ### # # # # # #### # # #"
      , "# #     ### # # # # # #    # # #"
      , "# # # # # # # # # # ######## # #"
      , "# # # #     #     #    #     # #"
      , "# ### ################ # # # # #"
      , "# #   #              # # #   # #"
      , "# # #### ############# # # # # #"
      , "# #                    #     # #"
      , "# # #### ############### # # # #"
      , "# #  # # #   #       ###     # #"
      , "# # ## # ### ### ### ### # ### #"
      , "# #          #   #   # # #   # #"
      , "# ################## # ### ### #"
      , "#                      #       #"
      , "################################"]


cellValid :: Cell -> (Int, Int) -> Bool
cellValid cell (pY, pX) = not $ cellVisited cell || (mapA !! pY !! pX) == '#'

data Walker =
    Walker { walkerPosY  :: !Int
           , walkerPosX  :: !Int
           , walkerDir   :: !Direction
           , walkerTurns :: !Int
           , walkerLastT :: !Bool
           , walkerCells :: ![[Cell]]
           } deriving (Show, Read)

walkerPos :: Walker -> (Int, Int)
walkerPos walker = (walkerPosY walker, walkerPosX walker)

walkerMove :: Walker -> (Int, Int) -> Walker
walkerMove walker (mY, mX) =
    walker {walkerPosY = pY - mY, walkerPosX = pX - mX}
  where (pY, pX) = walkerPos walker

walkerTurnRight :: Walker -> Walker
walkerTurnRight walker = walker { walkerDir =
    case (walkerDir walker) of
        DirUp    -> DirRight
        DirRight -> DirDown
        DirDown  -> DirLeft
        DirLeft  -> DirUp
    }

walkerTurnLeft :: Walker -> Walker
walkerTurnLeft walker = walker { walkerDir =
    case (walkerDir walker) of
        DirUp    -> DirLeft
        DirLeft  -> DirDown
        DirDown  -> DirRight
        DirRight -> DirUp
    }

walkerCell :: Walker -> Cell
walkerCell walker = (walkerCells walker) !! pY !! pX
  where (pY, pX) = walkerPos walker

walkerVisitCell :: Walker -> Walker
walkerVisitCell walker =
    walker { walkerCells = matrixChange cells pos $
            ((flip cellVisit dir) . walkerCell) walker }
  where
    pos   = walkerPos walker
    cells = walkerCells walker
    dir   = walkerDir walker

newWalker :: Walker
newWalker = Walker 1 1 DirDown 0 False $ map ( map (\c -> if c == '#'
                                                        then nonEmptyCell
                                                        else emptyCell)) mapA

walkerCellNotValid :: Walker -> Bool
walkerCellNotValid walker = cellVisited cell || dir `elem` (cellDirections cell)
  where
    cell = walkerCell walker
    dir  = walkerDir walker

walkerCollision :: Walker -> (Int, Int) -> Bool
walkerCollision walker (mY, mX)
    | pX >= mX  = False
    | pY >= mY  = False
    | pY < 0    = False
    | pX < 0    = False
    | otherwise = mapValid
  where
    (pY, pX) = walkerPos walker
    mapValid = not $ (mapA !! pY !! pX) == '#' || walkerCellNotValid walker

walkerAvailableCells :: Walker -> [(Direction, Cell)]
walkerAvailableCells walker =
    map (\(dir, cell) -> (dir, fromJust cell))
    $ filter (\(_, cell) -> isJust cell) $ cells
  where
    availableCells = [ (DirRight, ( 0,  1)) -- (DirRight, ( 0,  1)
                     , (DirUp,    (-1,  0))
                     , (DirLeft,  ( 0, -1))
                     , (DirDown,  ( 1,  0))
                     , (DirRight, ( 0,  0)) -- FIX ME PLEASE
                     ]
    (pY, pX) = walkerPos walker
    cells = map (\(dir, pos) ->
                (dir, matrixSafeSubscription (walkerCells walker) pos)) $
            map (\(dir, (mY, mX)) -> (dir, (mY + pY, mX + pX))) availableCells

walkerTurn :: Walker -> Walker
walkerTurn walker = if walkerLastT walker
                    then walkerTurnLeft updatedWalker
                    else walkerTurnRight updatedWalker
  where
    turns = walkerTurns walker
    oldDir = walkerLastT walker
    (newTurns, newDir) = if turns > 4 then (0, not oldDir) else (turns + 1, oldDir)
    updatedWalker = walker { walkerTurns = newTurns, walkerLastT = newDir }

walkerChangeDir :: Walker -> Walker
walkerChangeDir walker = if newPos /= walkerCell walker
                         then walker { walkerDir = newDir }
                         else walkerTurn walker
  where
    (mY, mX) = walkerPos walker
    pos direct = let (pY, pX) = directionDelta direct in (pY + mY, pX + mX)
    (newDir, newPos) = head $
                       filter (\(dir, cell) -> (\c -> cellValid c (pos dir) ) cell)
                              $ walkerAvailableCells walker

walkerPrintCell :: Walker -> [[Char]]
walkerPrintCell walker =
    [[ if cellVisited cell then '#' else ' '
        | i <- [0..width -1 ], let cell = (cells !! i) !! j ]
        | j <- [0..height -1 ]]
  where
    cells  = walkerCells walker
    width  = length $ cells !! 0
    height = length cells
