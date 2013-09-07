-----------------------------------------------------------------------------
--
-- Module      :  App.I18n
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

module App.I18n
    (
      I18n(..)
    , t
    ) where

import App.Message
import App.Color

data I18n
    = PosChanged (Int, Int)

t :: I18n -> Message
t str = case str of
    PosChanged (x, y) -> [ MLine "You just moved to (",
                           MColor CRed,   MLine $ show x,
                           MColor CWhite, MChar ',',
                           MColor CRed,   MLine $ show y,
                           MColor CWhite, MChar ')']
