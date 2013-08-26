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

module App.Panel (
    Panel (..)
  , newPanel
  , newLogger
  , panelPrint
  , panelPrintLn
  , panelAddCh
  , panelRefresh
  , panelClear
  , panelMvAdd
) where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

castEnum :: Char -> ChType
castEnum = toEnum . fromEnum

data Panel =
      Panel { panelSizes  :: !(Int, Int)
            , panelWindow :: !Window
            }
    | Logger { panelSizes :: !(Int, Int)
             , panelWindow :: !Window
             , loggerMessages :: [String]
             }

newPanel :: Int -> Int -> Int -> Int -> IO Panel
newPanel l c y x = newWin l c y x >>= \win -> return $ Panel (l, c) win

newLogger :: Int -> Int -> Int -> Int -> IO Panel
newLogger l c y x = newWin l c y x >>= \win -> return $ Logger (l, c) win []

panelPrint :: Panel -> String -> IO ()
panelPrint p = wAddStr $ panelWindow p

panelPrintLn :: Panel -> String -> IO ()
panelPrintLn p s = panelPrint p s >> panelAddCh p '\n'

panelAddCh :: Panel -> Char -> IO ()
panelAddCh p c = waddch (panelWindow p) (castEnum c) >>= (\_-> return ())

panelMvAdd :: Panel -> Int -> Int -> Char -> IO ()
panelMvAdd p y x c = (wMove (panelWindow p) y x) >> panelAddCh p c

panelClear :: Panel -> IO ()
panelClear p = wclear $ panelWindow p

panelRefresh :: Panel -> IO ()
panelRefresh p = wRefresh $ panelWindow p
