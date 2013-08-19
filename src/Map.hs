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
  , walkerPrintCell
) where

import Data.Maybe
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

matrixSafeSubscription :: [[a]] -> (Int, Int) -> Maybe a
matrixSafeSubscription arr (j, i) =
    if j >= 0 && j < height && i >= 0 && i < width
    then Just $ arr !! i !! j
    else Nothing
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
         } deriving (Show, Read, Eq)

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
newWalker = Walker 11 15 DirDown 0 True $ map ( map (\c -> if c == '#'
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

cellValid :: Cell -> (Int, Int) -> Bool
cellValid cell (pY, pX) = not $ cellVisited cell || (mapA !! pY !! pX) == '#'

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

performLogic :: Walker -> (Int, Int) -> Walker
performLogic walker sizes =
    if walkerCollision updatedWalker sizes
    then walkerVisitCell updatedWalker
    else walkerChangeDir walker
  where updatedWalker = walkerMove walker $ (directionDelta . walkerDir) walker
