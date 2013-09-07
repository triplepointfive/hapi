-----------------------------------------------------------------------------
--
-- Module      :  App.Message
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

module App.Message
    (
      Message
    , Line(..)
    , messageFromString
    , showMessage
    ) where

import App.Color

data Line
    = MLine    String
    | MChar    Char
    | MColor   Color
    | MNewLine
    deriving (Eq)

instance Show Line where
    show (MLine str)  = str
    show (MChar char) = [char]
    show MNewLine     = "\n"
    show (MColor _)   = ""

type Message = [Line]

messageFromString :: String -> Message
messageFromString s = [MLine s]

showMessage :: Message -> String
showMessage = foldr ((++).show) ""
