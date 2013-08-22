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
  , walkerTurn
  , makeCell
) where


import Data.Maybe
import Data.Functor ((<$>))
import Data.Char ( digitToInt, intToDigit )

import App.Cell
import App.Direction
import App.Matrix
import App.Stuff

mapA :: [[Char]]
mapA = ["################################"
      , "#                              #"
      , "##### ######################## #"
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
    walker {walkerPosY = pY + mY, walkerPosX = pX + mX}
  where (pY, pX) = walkerPos walker

walkerStep :: Walker -> Direction -> Walker
walkerStep walker dir = walkerMove (walkerVisitCell walker) $ directionDelta dir

walkerTurnRight :: Walker -> Walker
walkerTurnRight walker = walker { walkerDir = dirTurnRight $ walkerDir walker}

walkerTurnLeft :: Walker -> Walker
walkerTurnLeft walker = walker { walkerDir = dirTurnLeft $ walkerDir walker}

walkerCell :: Walker -> Cell
walkerCell walker = (walkerCells walker) ! (walkerPos walker)

walkerVisitCell :: Walker -> Walker
walkerVisitCell walker =
    walker { walkerCells = matrixChange cells pos $
            (cellVisit. walkerCell) walker }
  where
    pos   = walkerPos walker
    cells = walkerCells walker

newWalker :: (Int, Int) -> Matrix Char -> Direction -> Walker
newWalker (pY, pX) grid dir = Walker pY pX dir 0 False $
    map ( map makeCell) grid


makeCell :: Char -> Cell
makeCell c = case c of
    '#' -> nonEmptyCell
    ' ' -> emptyCell
    _   -> visitedCell $ digitToInt c

walkerCellNotValid :: Walker -> Bool
walkerCellNotValid walker = cellVisited $ walkerCell walker

walkerCollision :: Walker -> Bool
walkerCollision walker = if matrixValid mapA $ wPos
                         then mapValid
                         else False
  where
    wPos = walkerPos walker
    mapValid = not $ (mapA ! wPos) == '#' || walkerCellNotValid walker

walkerBestDir :: Walker -> Matrix Char -> Maybe Direction
walkerBestDir walker grid = fst <$> (listToMaybe $ sortWith (cellVisits.snd)
                                  $ walkerAvailableCells walker grid)

walkerAvailableCells :: Walker -> Matrix Char -> [(Direction, Cell)]
walkerAvailableCells walker grid = mapSnd snd $
    filter ((\(pos, cell) -> cellValid cell pos grid).snd) $
            mapSnd (\pos -> (pos, cells ! pos)) $
                  filter ((matrixValid cells).snd) $
                         mapSnd (pairSum wPos) dirsDeltas
  where
    cells = walkerCells walker
    wPos  = walkerPos walker

walkerTurn :: Walker -> Matrix Char -> Walker
walkerTurn walker grid = if isNothing bestDir
                         then walker
                         else walkerStep walker $ fromJust bestDir
  where bestDir = walkerBestDir walker grid

walkerChangeDir :: Walker -> Matrix Char -> Walker
walkerChangeDir walker grid = if isJust newDir
                         then walker { walkerDir = fromJust newDir }
                         else walker
  where newDir = walkerBestDir walker grid

walkerPrintCell :: Walker -> Matrix Char
walkerPrintCell walker =
    matrixIterate (walkerCells walker)
                  (\cell -> if cellVisited cell
                            then '#'
                            else intToDigit $ cellVisits cell)
