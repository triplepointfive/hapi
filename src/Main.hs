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
import App.Direction

castEnum :: Char -> ChType
castEnum = toEnum . fromEnum

moveAbout :: Walker -> Integer -> IO ()
moveAbout walker dTime = do
    erase -- clear curses's virtual screen but don't force a redraw
    wAddStr stdScr $ show $ walkerPos updatedWalker
    addLn
    addLn
    mapM_ (\line -> do
        mapM_ (\a ->
            waddch stdScr $ castEnum a
             ) line
        addLn) $ mapA
    mvAddCh (pY + 2) pX (castEnum '@') -- place a character in curses's virtual screen
    refresh -- copy the virtual screen to the terminal
    delay dTime
    c <- getch
    case cintToChar c of
        Just 'q' -> return ()
        Just 'a' -> moveAbout updatedWalker (dTime - 200)
        Just 'z' -> moveAbout updatedWalker (dTime + 200)
        Nothing  -> moveAbout updatedWalker dTime
        _        -> return ()
  where
    (pY, pX) = walkerPos updatedWalker
    updatedWalker = performLogic walker

main :: IO ()
main = do
    initCurses
    cBreak True
    noDelay stdScr True
    keypad stdScr True -- make the cursor keys usable
    echo False -- disable terminal echo
    _ <- cursSet CursorInvisible
    -- sizes <- scrSize
    moveAbout (newWalker (1, 1) mapA DirUp) 20000
    endWin
