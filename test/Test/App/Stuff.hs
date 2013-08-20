module Test.App.Stuff (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Stuff

tests :: TestTree
tests = testGroup "Stuff"
    [ itemsEqualOnDiffTests
    , mapCallTests
    ]

mapCallTests = testGroup "mapCall"
  [ testCase "call a method over all element in array" $
        mapCall (+) 0 [1,2,3] @?= 6
  ]

itemsEqualOnDiffTests = testGroup "itemsEqualOnDiff"
  [ testCase "return true for equal lists" $
        itemsEqualOnDiff [1,2,3] [3,1,2] @?= True
  , testCase "return false for non equal lists" $
        itemsEqualOnDiff [1,2,3] [3,2] @?= False
  ]
