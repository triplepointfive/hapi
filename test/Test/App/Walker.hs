module Test.App.Walker (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Walker
import App.Cell
import App.Direction

tests :: TestTree
tests = testGroup "Wlker"
    [ walkerBestDirTests
    ]

testWalker = newWalker (1, 1)

walkerBestDirTests = testGroup "walkerBestDir"
  [ testCase "walker best choice go up" $
        (walkerBestDir $ testWalker ["###",
                                     "# #",
                                     "# #"]) @?= (Just DirUp)
  , testCase "can't go in any directions" $
        (walkerBestDir $ testWalker ["###",
                                     "# #",
                                     "###"]) @?= Nothing
  ]
