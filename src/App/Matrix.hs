-----------------------------------------------------------------------------
--
-- Module      :  App.Matrix
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

module App.Matrix (
    Matrix
  , matrixChange
  , matrixSafeSubscription
  , matrixSizes
  , matrixValid
  , (!)
) where

type Matrix a = [[a]]

infix 9 !
(!) :: Matrix a -> (Int,Int) -> a
m ! (i,j) = m !! (i) !! (j)

matrixChange :: Matrix a -> (Int, Int) -> a -> Matrix a
matrixChange arr (pY, pX) el =
    [[ if i == pY && j == pX then el else arr ! (i, j)
        | i <- [0..height]]
        | j <- [0..width]]
  where (height, width) = matrixSizes arr

matrixSafeSubscription :: Matrix a -> (Int, Int) -> Maybe a
matrixSafeSubscription arr coord = if matrixValid arr coord
                                   then Just $ arr ! coord
                                   else Nothing

matrixSizes :: Matrix a -> (Int, Int)
matrixSizes arr = (length arr, length $ arr !! 0)

matrixValid :: Matrix a -> (Int, Int) -> Bool
matrixValid arr (i, j) = i >= 0 && i < height && j >= 0 && j < width
  where (height, width) = matrixSizes arr
