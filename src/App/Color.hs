-----------------------------------------------------------------------------
--
-- Module      :  App.Color
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

module App.Color
    (
      Color(..)
    , colorsInitialize
    , colorToExternal
    ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

data Color
  = CRed
  | CWhite
  | CBlack
  | CBlue
  | CGreen
  | CYellow
  | CMagenta
  | CCyan
  deriving (Show, Eq)

colorToExternal :: Color -> Curses.Pair
colorToExternal color = case color of
    CRed     -> Curses.Pair 1
    CWhite   -> Curses.Pair 2
    CBlack   -> Curses.Pair 3
    CBlue    -> Curses.Pair 4
    CGreen   -> Curses.Pair 5
    CYellow  -> Curses.Pair 6
    CMagenta -> Curses.Pair 7
    CCyan    -> Curses.Pair 8

colorsInitialize :: IO ()
colorsInitialize = do
    Curses.startColor
    Curses.initPair (Curses.Pair 1) CursesH.red     CursesH.black
    Curses.initPair (Curses.Pair 2) CursesH.white   CursesH.black
    Curses.initPair (Curses.Pair 3) CursesH.black   CursesH.black
    Curses.initPair (Curses.Pair 4) CursesH.blue    CursesH.black
    Curses.initPair (Curses.Pair 5) CursesH.green   CursesH.black
    Curses.initPair (Curses.Pair 6) CursesH.yellow  CursesH.black
    Curses.initPair (Curses.Pair 7) CursesH.magenta CursesH.black
    Curses.initPair (Curses.Pair 8) CursesH.cyan    CursesH.black

