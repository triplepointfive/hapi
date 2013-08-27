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

import qualified Foreign.C.Types (CInt)

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Concurrent.Thread.Delay

import App.Keys
import App.Map
import App.Enemy
import App.Direction
import App.Panel
import App.Walker
import Control.Monad.State

data App = App { appPanels :: ![Panel]
               , appWalker :: !Enemy
               , appDelay  :: !Integer
               }


loop :: StateT App IO ()
loop = do
    app <- get
    (c, updatedWalker) <- io $ interactive app
    put $ app {appWalker = updatedWalker }
    case cintToChar c of
        Just 'q' -> return ()
    --        Just 'a' -> loop $ nApp {appDelay = dTime - 200}
    --      Just 'z' -> loop $ nApp {appDelay = dTime + 200}
        Nothing  -> loop
        _        -> return ()
  --where dTime   = appDelay app

interactive :: App -> IO ((Foreign.C.Types.CInt, Enemy))
interactive app = do
    panelClear rootWin
    mapM_ (panelPrintLn rootWin) $ walkerPrintCell updatedWalker
    panelColorSet rootWin 1
    panelMvAdd rootWin pY pX '@'
    panelResetStyle rootWin
    panelRefresh rootWin
    delay dTime
    c <- getch
    return $ (c, updatedWalker)
  where
    (pY, pX) = walkerPos updatedWalker
    updatedWalker = performLogic $ appWalker app
    rootWin = appPanels app !! 0
    logWin  = appPanels app !! 1
    dTime   = appDelay app

initialize :: IO App
initialize = do
    initCurses
    startColor
    initPair (Pair 1) red black
    cBreak True
    noDelay stdScr True
    keypad stdScr True
    echo False
    _ <- cursSet CursorInvisible
    (mY, mX) <- scrSize
    rootWin <- newPanel (mY - 10) (mX - 30) 0 0
    sidebarWin <- newPanel (mY - 10) 30 0 (mX - 30)
    logWin <- newLogger 10 mX (mY - 10) 0
    refresh
    return $ App [rootWin, logWin, sidebarWin] (newEnemy (1, 1) mapA DirUp) 20000

clear :: App -> IO ()
clear app = do
    mapM_ (delWin.panelWindow) $ appPanels app
    endWin

io :: IO a -> StateT App IO a
io = liftIO

main :: IO ()
main = do
    app <- initialize
    nApp <- runStateT loop app
    clear $ snd nApp
