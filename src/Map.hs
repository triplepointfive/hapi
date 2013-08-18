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

module Map (
    Walker (..)
  , walkerPos
  , performLogic
  , newWalker
  , mapA
) where

import Data.List (delete)

itemsEqualOnDiff :: (Eq a) => [a] -> [a] -> Bool
itemsEqualOnDiff a b = null $ absDiff a b

absDiff :: (Eq a) => [a] -> [a] -> [a]
absDiff [] [] = []
absDiff [] b = b
absDiff a [] = a
absDiff (aHead:aTail) b = absDiff aTail $ delete aHead b

matrixChange :: [[a]] -> (Int, Int) -> a -> [[a]]
matrixChange arr (pY, pX) el =
    [[ if i == pY && j == pX then el else arr !! i !! j
        | i <- [0..height]]
        | j <- [0..width]]
  where
    width  = length $ arr !! 0
    height = length arr


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

data Cell =
    Cell { cellVisited :: !Bool
         , cellDirections :: ![Direction]
         } deriving (Show, Read)

emptyCell :: Cell
emptyCell = Cell False []

nonEmptyCell :: Cell
nonEmptyCell = Cell True []

cellVisit :: Cell -> Direction -> Cell
cellVisit cell@(Cell True _) _ = cell
cellVisit orig@(Cell False directions) dir =
    if itemsEqualOnDiff directions dirs
    then cell { cellVisited = True }
    else cell
  where cell = orig { cellDirections = dir : directions }

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

data Walker =
    Walker { walkerPosY  :: !Int
           , walkerPosX  :: !Int
           , walkerDir   :: !Direction
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
newWalker = Walker 1 1 DirDown $ map ( map (\c -> if c == '#'
                                                   then nonEmptyCell
                                                   else emptyCell)) mapA

walkerCellValid :: Walker -> Bool
walkerCellValid walker = cellVisited cell || dir `elem` (cellDirections cell)
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
    mapValid = not $ (mapA !! pY !! pX) == '#' || walkerCellValid walker

performLogic :: Walker -> (Int, Int) -> Walker
performLogic walker sizes =
    if walkerCollision updatedWalker sizes
    then walkerVisitCell updatedWalker
    else walkerTurnLeft walker
  where updatedWalker = walkerMove walker $ (directionDelta . walkerDir) walker
