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
) where

type Matrix a = [[a]]

matrixChange :: Matrix a -> (Int, Int) -> a -> Matrix a
matrixChange arr (pY, pX) el =
    [[ if i == pY && j == pX then el else arr !! i !! j
        | i <- [0..height]]
        | j <- [0..width]]
  where
    width  = length $ arr !! 0
    height = length arr

matrixSafeSubscription :: Matrix a -> (Int, Int) -> Maybe a
matrixSafeSubscription arr (j, i) =
    if j >= 0 && j < height && i >= 0 && i < width
    then Just $ arr !! i !! j
    else Nothing
  where
    width  = length $ arr !! 0
    height = length arr
