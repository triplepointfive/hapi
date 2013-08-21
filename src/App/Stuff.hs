-----------------------------------------------------------------------------
--
-- Module      :  App.Stuff
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

module App.Stuff (
    itemsEqualOnDiff
  , mapCall
  , pairSum
  , mapResult
  , mapSnd
  , filterSnd
  , sortWith
  , applySnd
) where

import Data.List (intersect, sortBy)

itemsEqualOnDiff :: (Eq a) => [a] -> [a] -> Bool
itemsEqualOnDiff a b = a == a `intersect` b

mapCall :: (a -> b -> a) -> a -> [b] -> a
mapCall _ a []     = a
mapCall f a (b:bs) = mapCall f (f a b) bs

pairSum :: (Num a) => (a, a) -> (a, a) -> (a, a)
pairSum (mY, mX) (pY, pX) = (mY + pY, mX + pX)

mapResult :: (a -> b) -> [a] -> [(a, b)]
mapResult f = map (\a -> (a, f a))

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map (\(c, a) -> (c, f a))

filterSnd :: (a -> Bool) -> [(c, a)] -> [(c, a)]
filterSnd f = filter (\(_, a) -> f a)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

applySnd :: (a -> b) -> a -> (a, b)
applySnd f a = (a, f a)
