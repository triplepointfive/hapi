module Test.App.Logger (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as Vector

import App.Logger

tests :: TestTree
tests = testGroup "Logger"
    [ loggerAddMessageTests
    , loggerNewestMessagesTests
    ]

loggerAddMessageTests = testGroup "loggerAddMessage"
  [ testCase "add message to new logger" $
        Vector.length (loggerMessages $ loggerAddMessage newLogger "Hello, Logger!") @?= 1
  , testCase "add messages to the beginning of list" $
        loggerMessages (loggerAddMessage (loggerAddMessage newLogger "1") "2")
            @?= Vector.fromList ["2", "1"]
  ]

loggerNewestMessagesTests = testGroup "loggerNewestMessages"
  [ testCase "should return last messages" $
        loggerNewestMessages
            ( foldl loggerAddMessage newLogger ["1", "2","3", "4", "5", "6", "7"] ) 5
            @?= ["7","6","5","4","3"]
  ]
