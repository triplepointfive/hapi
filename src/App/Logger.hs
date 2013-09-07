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

module App.Logger
    (
      Logger (..)
    , newLogger
    , loggerAddMessage
    , loggerNewestMessages
    , loggerPrintLastMessages
    , loggerAddString
    ) where

import qualified Data.Vector as Vector

import App.Panel
import App.Message

data Logger = Logger
    { loggerMessages :: Vector.Vector Message
    }

newLogger :: Logger
newLogger = Logger Vector.empty

loggerPrintLastMessages :: Logger -> Panel -> IO ()
loggerPrintLastMessages l p = do
    panelClear p
    mapM_ (panelPrintMessage p) $ Vector.toList $ Vector.take ((fst.panelSizes) p) (loggerMessages l)
    panelRefresh p

loggerAddMessage :: Logger -> Message -> Logger
loggerAddMessage l mes = l { loggerMessages = Vector.cons mes (loggerMessages l) }

loggerAddString :: Logger -> String -> Logger
loggerAddString l mes = l { loggerMessages = Vector.cons (messageFromString mes) (loggerMessages l) }

loggerNewestMessages :: Logger -> Int -> [Message]
loggerNewestMessages l n = Vector.toList $ Vector.take n $ loggerMessages l
