module Test.App.Logger (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as Vector

import App.Logger
import App.Message

tests :: TestTree
tests = testGroup "Logger"
    [ loggerAddMessageTests
    , loggerNewestMessagesTests
    ]

loggerAddMessageTests = testGroup "loggerAddString"
  [ testCase "add message to new logger" $
        Vector.length (loggerMessages $ loggerAddString newLogger "Hello, Logger!") @?= 1
  , testCase "add messages to the beginning of list" $
        loggerMessages (loggerAddString (loggerAddString newLogger "1") "2")
            @?= Vector.fromList (map messageFromString ["2", "1"])
  ]

loggerNewestMessagesTests = testGroup "loggerNewestMessages"
  [ testCase "should return last messages" $
        loggerNewestMessages
            ( foldl loggerAddString newLogger ["1", "2","3", "4", "5", "6", "7"] ) 5
            @?= map messageFromString ["7","6","5","4","3"]
  ]
