-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Concurrent.Thread.Delay

import App.Keys
import App.Map
import App.Walker
import App.Direction

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

moveAbout :: Walker -> Integer -> [Panel] -> IO ()
moveAbout walker dTime windows = do
    panelClear rootWin
    mapM_ (panelPrintLn rootWin) mapA
    -- attrSet attr0 (Pair 1)
    panelMvAdd rootWin pY pX '@'
    -- resetStyle
    panelRefresh rootWin
    delay dTime
    c <- getch
    case cintToChar c of
        Just 'q' -> return ()
        Just 'a' -> moveAbout updatedWalker (dTime - 200) windows
        Just 'z' -> moveAbout updatedWalker (dTime + 200) windows
        Nothing  -> moveAbout updatedWalker dTime windows
        _        -> return ()
  where
    (pY, pX) = walkerPos updatedWalker
    updatedWalker = performLogic walker
    rootWin = windows !! 0
    logWin  = windows !! 1

main :: IO ()
main = do
    initCurses
    initPair (Pair 1) red black
    startColor
    cBreak True
    noDelay stdScr True
    keypad stdScr True -- make the cursor keys usable
    echo False -- disable terminal echo
    _ <- cursSet CursorInvisible
    (mY, mX) <- scrSize
    rootWin <- newPanel (mY - 10) (mX - 30) 0 0
    sidebarWin <- newPanel (mY - 10) 30 0 (mX - 30)
    logWin <- newLogger 10 mX (mY - 10) 0
    refresh

    moveAbout (newWalker (1, 1) mapA DirUp) 20000 [rootWin, logWin]
    delWin $ panelWindow rootWin
    delWin $ panelWindow logWin
    delWin $ panelWindow sidebarWin
    endWin
