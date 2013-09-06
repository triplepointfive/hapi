-----------------------------------------------------------------------------
--
-- Module      :  App.Logger
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

module App.Logger (
    Logger (..)
  , newLogger
  , loggerAddMessage
  , loggerNewestMessages
  , loggerPrintLastMessages
) where

import qualified Data.Vector as Vector

import App.Panel

type Message = String

data Logger =
    Logger {
             loggerMessages :: Vector.Vector String
           }

newLogger :: Logger
newLogger = Logger Vector.empty

loggerPrintLastMessages :: Logger -> Panel -> IO ()
loggerPrintLastMessages l p = do
    panelClear p
    mapM_ (panelPrintLn p) $ Vector.toList $ Vector.take ((fst.panelSizes) p) (loggerMessages l)
    panelRefresh p

loggerAddMessage :: Logger -> Message -> Logger
loggerAddMessage l mes = l { loggerMessages = Vector.cons mes (loggerMessages l) }

loggerNewestMessages :: Logger -> Int -> [Message]
loggerNewestMessages l n = Vector.toList $ Vector.take n $ loggerMessages l
