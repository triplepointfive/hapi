module Test.App.Stuff (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Stuff

tests :: TestTree
tests = testGroup "Stuff"
    [ itemsEqualOnDiffTests
    , mapCallTests
    , pairSumTests
    , mapResultTests
    , mapSndTests
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

pairSumTests = testGroup "pairSum"
  [  testCase "sum pair element" $
        pairSum (1, 2) (3, 4) @?= (4, 6)
  ]

mapResultTests = testGroup "mapResult"
  [  testCase "should map over list and save arguments" $
        mapResult even [1,2,3] @?= [(1, False), (2, True), (3, False)]
  ]

mapSndTests = testGroup "mapSnd"
  [  testCase "map over snd list elems" $
        mapSnd (+1) [(1,2), (2, 4)] @?= [(1,3),(2,5)]
  ]
