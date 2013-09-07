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

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module App.Walker
    (
      Walker (..)
    , walkerVisitCell
    , walkerChangeDir
    , walkerMove
    , walkerPrintCell
    , walkerBestDir
    , walkerTurn
    , walkerStep
    , walkerTryMove
    ) where

import qualified Data.Char as Char
import qualified Data.Functor as Functor
import qualified Data.Maybe as Maybe

import App.Cell
import App.Direction
import App.Matrix
import App.Stuff

class Walker a where
    walkerPosX     :: a -> Int
    walkerPosY     :: a -> Int
    walkerPos      :: a -> (Int, Int)
    walkerSetPos   :: a -> (Int, Int) -> a
    walkerCells    :: a -> [[Cell]]
    walkerSetCells :: a -> [[Cell]] -> a
    walkerSetDir   :: a -> Direction -> a

cellValid :: Cell -> (Int, Int) -> Matrix Char -> Bool
cellValid cell coords grid = not $ cellVisited cell || (grid ! coords) == '#'

walkerMove :: (Walker a) => a -> (Int, Int) -> a
walkerMove walker (mY, mX) = walkerSetPos walker (pY + mY, pX + mX)
  where (pY, pX) = walkerPos walker

walkerStep :: (Walker a) => a -> Direction -> a
walkerStep walker dir = walkerMove (walkerVisitCell walker) $ directionDelta dir

walkerCell :: (Walker a) => a -> Cell
walkerCell walker = walkerCells walker ! walkerPos walker

walkerVisitCell :: (Walker a) => a -> a
walkerVisitCell walker = walkerSetCells walker $ matrixChange (walkerCells walker)
                            (walkerPos walker) $ (cellVisit. walkerCell) walker

walkerBestDir :: (Walker a) => a -> Matrix Char -> Maybe Direction
walkerBestDir walker grid = fst Functor.<$> Maybe.listToMaybe (sortWith (cellVisits.snd)
                                  $ walkerAvailableCells walker grid)

walkerTryMove :: (Walker a) => a -> Matrix Char -> Direction -> a
walkerTryMove walker grid dir = if dir `elem` map fst (walkerAvailableCells walker grid)
                                then walkerStep walker dir
                                else walker

walkerAvailableCells :: (Walker a) => a -> Matrix Char -> [(Direction, Cell)]
walkerAvailableCells walker grid = mapSnd snd $
    filter ((\(pos, cell) -> cellValid cell pos grid).snd) $
            mapSnd (\pos -> (pos, cells ! pos)) $
                  filter (matrixValid cells.snd) $
                         mapSnd (pairSum wPos) dirsDeltas
  where
    cells = walkerCells walker
    wPos  = walkerPos walker

walkerTurn :: (Walker a) => a -> Matrix Char -> a
walkerTurn walker grid = maybe walker (walkerStep walker) bestDir
  where bestDir = walkerBestDir walker grid

walkerChangeDir :: (Walker a ) => a -> Matrix Char -> a
walkerChangeDir walker grid = maybe walker (walkerSetDir walker) newDir
  where newDir = walkerBestDir walker grid

walkerPrintCell :: (Walker a) => a -> Matrix Char
walkerPrintCell walker =
    matrixIterate (walkerCells walker)
                  (\cell -> if cellVisited cell
                            then '#'
                            else if cellVisits cell == 0
                                 then ' '
                                 else Char.intToDigit $ cellVisits cell)
