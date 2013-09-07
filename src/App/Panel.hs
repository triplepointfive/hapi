-----------------------------------------------------------------------------
--
-- Module      :  App.Panel
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

module App.Panel
    (
      Panel (..)
    , newPanel
    , panelPrint
    , panelPrintLn
    , panelAddCh
    , panelRefresh
    , panelClear
    , panelMvAdd
    , panelColorSet
    , panelResetStyle
    , panelPrintMessage
    ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import App.Color
import App.Message

castEnum :: Char -> Curses.ChType
castEnum = toEnum . fromEnum

data Panel = Panel
    { panelSizes  :: !(Int, Int)
    , panelWindow :: !Curses.Window
    }

newPanel :: Int -> Int -> Int -> Int -> IO Panel
newPanel l c y x = Curses.newWin l c y x >>= \win -> return $ Panel (l, c) win

panelPrint :: Panel -> String -> IO ()
panelPrint p = Curses.wAddStr $ panelWindow p

panelPrintMessage :: Panel -> Message -> IO ()
panelPrintMessage p m = panelResetStyle p >> mapM_ (panelProcessLine p) m >> panelNewLine p

panelProcessLine :: Panel -> Line -> IO ()
panelProcessLine p l = case l of
    MLine str  -> panelPrint p str
    MChar char -> panelAddCh p char
    MColor col -> panelColorSet p col
    MNewLine   -> panelNewLine p

panelPrintLn :: Panel -> String -> IO ()
panelPrintLn p s = panelPrint p s >> panelNewLine p

panelNewLine :: Panel -> IO ()
panelNewLine p = panelAddCh p '\n'

panelAddCh :: Panel -> Char -> IO ()
panelAddCh p c = Curses.waddch (panelWindow p) (castEnum c) >>= (\_-> return ())

panelMvAdd :: Panel -> Int -> Int -> Char -> IO ()
panelMvAdd p y x c = Curses.wMove (panelWindow p) y x >> panelAddCh p c

panelClear :: Panel -> IO ()
panelClear p = Curses.wclear $ panelWindow p

panelRefresh :: Panel -> IO ()
panelRefresh p = Curses.wRefresh $ panelWindow p

panelColorSet :: Panel -> Color -> IO ()
panelColorSet p c = Curses.wAttrSet (panelWindow p) (Curses.attr0, colorToExternal c)

panelResetStyle :: Panel -> IO ()
panelResetStyle p = CursesH.wResetStyle $ panelWindow p
