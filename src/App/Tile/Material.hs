-----------------------------------------------------------------------------
--
-- Module      :  App.Tile.Material
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

module App.Tile.Material
    (
      Material (..)
    , materialColor
    ) where

import App.Color

data Material
  = MWooden
  | MStone
  deriving (Show, Eq)

materialColor :: Material -> Color
materialColor mat = case mat of
    MWooden -> CRed
    MStone  -> CWhite
