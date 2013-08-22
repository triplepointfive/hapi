module Test.App.Matrix (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Matrix

tests :: TestTree
tests = testGroup "Matrix"
    [ matrixSizesTests
    , matrixValidTests
    , matrixIterateTests
    ]

matrixSizesTests = testGroup "matrixSizes"
  [ testCase "call a method over all element in array" $
        matrixSizes [[1,2,3],[1,2,3]] @?= (2, 3)
  ]

matrixValidTests = testGroup "matrixValid"
  [ testCase "valid coordinates" $
        matrixValid [[1,2,3],[1,2,3]] (1, 2) @?= True
  , testCase "out of ranges" $
        matrixValid [[1,2,3],[1,2,3]] (2, 2) @?= False
  ]

matrixIterateTests = testGroup "matrixIterate"
  [ testCase "apply function on all matrix elements" $
        matrixIterate [[1,2,3],[5,6,7]] (+1) @?= [[2,3,4],[6,7,8]]
  ]

