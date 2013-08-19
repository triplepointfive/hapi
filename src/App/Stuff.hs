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
) where

import Data.List (intersect)

itemsEqualOnDiff :: (Eq a) => [a] -> [a] -> Bool
itemsEqualOnDiff a b = a == a `intersect` b

mapCall :: (a -> b -> a) -> a -> [b] -> a
mapCall _ a []     = a
mapCall f a (b:bs) = mapCall f (f a b) bs
