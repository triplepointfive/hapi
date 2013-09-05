module Test.App.Logger (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Logger

tests :: TestTree
tests = testGroup "Logger"
    [ matrixSizesTests
    , matrixValidTests
    , matrixIterateTests
    , matrixChangeTests
    ]

matrixSizesTests = testGroup "matrixSizes"
  [ testCase "call a method over all element in array" $
        matrixSizes [[1,2,3],[1,2,3]] @?= (2, 3)
  ]
