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
{-# LANGUAGE CPP #-}

module App.Logger (
    Logger (..)
  , newLogger
  , loggerAddMessage
  , loggerNewestMessages
) where

import qualified Data.Vector as Vector

import App.Panel

type Message = String

data Logger =
    Logger {
#ifndef TEST_MODE
             loggerPanel    :: !Panel
           ,
#endif
             loggerMessages :: Vector.Vector String
           }

newLogger
#ifndef TEST_MODE
          :: Int
          -> Int
          -> Int
          -> Int
          -> IO Logger
newLogger l c y x = newPanel l c y x >>= \panel -> return $ Logger panel Vector.empty
#else
          :: Logger
newLogger = Logger Vector.empty
#endif

loggerAddMessage :: Logger -> Message -> Logger
loggerAddMessage l mes = l { loggerMessages = Vector.cons mes (loggerMessages l) }

loggerNewestMessages :: Logger -> Int -> [Message]
loggerNewestMessages l n = Vector.toList $ Vector.take n $ loggerMessages l

