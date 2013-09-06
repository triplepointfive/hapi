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

import Control.Monad.State

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import App.Map
import App.Horus
import App.Direction
import App.Panel
import App.Walker
import App.Logger
import App.UserInput

data App = App { appPanels :: ![Panel]
               , appLogger :: !Logger
               , appHorus  :: !Horus
               , appDelay  :: !Integer
               }

loop :: StateT App IO ()
loop = do
    app <- get
    io $ appOutput app
    inp <- io $ getInput app
    case inp of
        Just Exit -> return ()
        Just (Move dir) -> do
            put $ app {appHorus = walkerTryMove (appHorus app) mapA dir}
            loop
        _ -> loop

getInput :: App -> IO (Maybe UserInput)
getInput app = do
    c <- getch
    return $ processInput c

appOutput :: App -> IO ()
appOutput app = do
    panelClear rootWin
    mapM_ (panelPrintLn rootWin) $ walkerPrintCell walker
    panelColorSet rootWin 1
    panelMvAdd rootWin pY pX '@'
    panelResetStyle rootWin
    panelRefresh rootWin
    loggerPrintLastMessages (appLogger app) logWin
  where
    walker   = appHorus app
    (pY, pX) = walkerPos walker
    rootWin = appPanels app !! 0
    logWin  = appPanels app !! 1

initialize :: IO App
initialize = do
    initCurses
    startColor
    initPair (Pair 1) red black
    cBreak True
    -- noDelay stdScr True
    keypad stdScr True
    echo False
    _ <- cursSet CursorInvisible
    (mY, mX) <- scrSize
    rootWin <- newPanel (mY - 10) (mX - 30) 0 0
    sidebarWin <- newPanel (mY - 10) 30 0 (mX - 30)
    logWin <- newPanel 10 mX (mY - 10) 0
    refresh
    return $ App [rootWin, logWin, sidebarWin] (newLogger) (newHorus (1, 1) mapA DirUp) 20000

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
