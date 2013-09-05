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
) where



import App.Panel

type Message = String

data Logger =
    Logger { loggerPanel    :: !Panel
           , loggerMessages :: [String]
           }

newLogger :: Int -> Int -> Int -> Int -> IO Logger
newLogger l c y x = newPanel l c y x >>= \panel -> return $ Logger panel []

loggerAddMessage :: [Message]
