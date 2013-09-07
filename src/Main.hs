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

import qualified Control.Monad.State as State

import qualified UI.HSCurses.Curses as Curses

import App.Map
import App.Horus
import App.Direction
import App.Panel
import App.Walker
import App.Logger
import App.UserInput
import App.Color
import qualified App.I18n as I18n

data App = App
    { appPanels :: ![Panel]
    , appLogger :: !Logger
    , appHorus  :: !Horus
    , appDelay  :: !Integer
    }

loop :: State.StateT App IO ()
loop = do
    app <- State.get
    io $ appOutput app
    inp <- io $ getInput app
    case inp of
        Just Exit -> return ()
        Just (Move dir) -> do
            State.put $ app
                {appHorus = walkerTryMove (appHorus app) mapA dir,
                 appLogger = loggerAddMessage (appLogger app)
                                              $ I18n.t (I18n.PosChanged $ walkerPos $ appHorus app) }
            loop
        _ -> loop

getInput :: App -> IO (Maybe UserInput)
getInput _ = do
    c <- Curses.getch
    return $ processInput c

appOutput :: App -> IO ()
appOutput app = do
    panelClear rootWin
    mapM_ (panelPrintLn rootWin) $ walkerPrintCell walker
    panelColorSet rootWin CCyan
    panelMvAdd rootWin pY pX '@'
    panelResetStyle rootWin
    panelRefresh rootWin
    loggerPrintLastMessages (appLogger app) logWin
  where
    walker   = appHorus app
    (pY, pX) = walkerPos walker
    rootWin = head (appPanels app)
    logWin  = appPanels app !! 1

initialize :: IO App
initialize = do
    Curses.initCurses
    colorsInitialize
    Curses.cBreak True
    -- noDelay stdScr True
    Curses.keypad Curses.stdScr True
    Curses.echo False
    _ <- Curses.cursSet Curses.CursorInvisible
    (mY, mX) <- Curses.scrSize
    rootWin <- newPanel (mY - 10) (mX - 30) 0 0
    sidebarWin <- newPanel (mY - 10) 30 0 (mX - 30)
    logWin <- newPanel 10 mX (mY - 10) 0
    Curses.refresh
    return $ App [rootWin, logWin, sidebarWin] newLogger (newHorus (1, 1) mapA DirUp) 20000

clear :: App -> IO ()
clear app = do
    mapM_ (Curses.delWin.panelWindow) $ appPanels app
    Curses.endWin

io :: IO a -> State.StateT App IO a
io = State.liftIO

main :: IO ()
main = do
    app <- initialize
    nApp <- State.runStateT loop app
    clear $ snd nApp
