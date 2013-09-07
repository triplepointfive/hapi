-----------------------------------------------------------------------------
--
-- Module      :  App.Input
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

module App.UserInput
    (
      UserInput (..)
    , processInput
    ) where

import qualified Foreign.C.Types (CInt)

import App.Keys
import App.Direction

data UserInput
    = Move Direction
    | Exit
    | Unknown
    deriving (Show, Eq)

processInput :: Foreign.C.Types.CInt -> Maybe UserInput
processInput c = case cintToChar c of
    Just 'q' -> Just Exit
    Just 'h' -> Just $ Move DirLeft
    Just 'j' -> Just $ Move DirUp
    Just 'k' -> Just $ Move DirDown
    Just 'l' -> Just $ Move DirRight
    Nothing  -> Nothing
    _        -> Just Unknown
