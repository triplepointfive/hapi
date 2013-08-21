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
  , walkerBestDir
) where


import Data.Maybe
import Data.List
import Data.Functor ((<$>))

import App.Cell
import App.Direction
import App.Matrix
import App.Stuff

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


cellValid :: Cell -> (Int, Int) -> Matrix Char -> Bool
cellValid cell coords grid = not $ cellVisited cell || (grid ! coords) == '#'

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
walkerTurnRight walker = walker { walkerDir = dirTurnRight $ walkerDir walker}

walkerTurnLeft :: Walker -> Walker
walkerTurnLeft walker = walker { walkerDir = dirTurnLeft $ walkerDir walker}

walkerCell :: Walker -> Cell
walkerCell walker = (walkerCells walker) ! (walkerPos walker)

walkerVisitCell :: Walker -> Walker
walkerVisitCell walker =
    walker { walkerCells = matrixChange cells pos $
            ((flip cellVisit dir) . walkerCell) walker }
  where
    pos   = walkerPos walker
    cells = walkerCells walker
    dir   = walkerDir walker

newWalker :: (Int, Int) -> Matrix Char -> Walker
newWalker (pY, pX) grid = Walker pY pX DirUp 0 False $
    map ( map (\c -> if c == '#' then nonEmptyCell else emptyCell)) grid

walkerCellNotValid :: Walker -> Bool
walkerCellNotValid walker = cellVisited cell || dir `elem` (cellDirections cell)
  where
    cell = walkerCell walker
    dir  = walkerDir walker

walkerCollision :: Walker -> Bool
walkerCollision walker = if matrixValid mapA $ wPos
                         then mapValid
                         else False
  where
    wPos = walkerPos walker
    mapValid = not $ (mapA ! wPos) == '#' || walkerCellNotValid walker

walkerBestDir :: Walker -> Maybe Direction
walkerBestDir walker = fst <$> (listToMaybe $ sortWith (length.cellDirections.snd)
                                  $ walkerAvailableCells walker mapA)

walkerAvailableCells :: Walker -> Matrix Char -> [(Direction, Cell)]
walkerAvailableCells walker grid = mapSnd snd $
    filter ((\(pos, cell) -> cellValid cell pos grid).snd) $
            mapSnd (\pos -> (pos, cells ! pos)) $
                  filter ((matrixValid cells).snd) $
                         mapSnd (pairSum wPos) dirsDeltas
  where
    cells = walkerCells walker
    wPos  = walkerPos walker

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
walkerChangeDir walker = walker
{-if newPos /= walkerCell walker
                         then walker { walkerDir = newDir }
                         else walkerTurn walker
  where
    pos direct = pairSum (walkerPos walker) $ directionDelta direct
    (newDir, newPos) = fromJust $ find (\(dir, cell) ->
                                        (\c -> cellValid c (pos dir) ) cell)
                              $ walkerAvailableCells walker mapA
-}
walkerPrintCell :: Walker -> Matrix Char
walkerPrintCell walker =
    [[ if cellVisited cell then '#' else ' '
        | i <- [0..width -1 ], let cell = cells ! (i, j) ]
        | j <- [0..height -1 ]]
  where
    cells  = walkerCells walker
    (height, width) = matrixSizes cells
