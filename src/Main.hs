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
import Control.Concurrent.Thread.Delay

import App.Keys
import App.Map
import App.Walker

castEnum :: Char -> ChType
castEnum = toEnum . fromEnum

moveAbout :: Walker -> (Int, Int) -> Integer -> IO ()
moveAbout walker sizes dTime = do
    erase -- clear curses's virtual screen but don't force a redraw
    mapM_ (\line -> do
        mapM_ (\a -> waddch stdScr $ castEnum a) line
        waddch stdScr $ castEnum '\n') mapA
    mvAddCh pY pX (castEnum '@') -- place a character in curses's virtual screen
    refresh -- copy the virtual screen to the terminal
    delay dTime
    c <- getch
    case cintToChar c of
        Just 'q' -> return ()
        Just 'a' -> moveAbout updatedWalker sizes (dTime - 200)
        Just 'z' -> moveAbout updatedWalker sizes (dTime + 200)
        Nothing  -> moveAbout updatedWalker sizes dTime
        _        -> return ()
  where
    (pY, pX) = walkerPos walker
    updatedWalker = performLogic walker sizes

main :: IO ()
main = do
    initCurses
    cBreak True
    noDelay stdScr True
    keypad stdScr True -- make the cursor keys usable
    echo False -- disable terminal echo
    _ <- cursSet CursorInvisible
    sizes <- scrSize
    moveAbout newWalker sizes 20000
    endWin
