module Test.App.Walker (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Walker
import App.Enemy
import App.Direction
import App.Matrix
import App.Cell

tests :: TestTree
tests = testGroup "Walker"
    [ walkerBestDirTests
    , walkerCellsTests
    ]

testWalker = newEnemy (1, 1)

walkerBestDirTests = testGroup "walkerBestDir"
  [ testCase "walker best choice go up" $
        let grid = ["###",
                    "# #",
                    "# #"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= (Just DirUp)
  , testCase "can't go in any directions" $
        let grid = ["###",
                    "# #",
                    "###"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= Nothing
  , testCase "should want go on empty cell" $
        let grid = ["####",
                    "#  #",
                    "####"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= Just DirRight
  , testCase "should want go back on deadlock" $
        let grid = ["####",
                    "#  #",
                    "####"] in
        (walkerBestDir (
         walkerTurn (testWalker grid DirUp) grid) grid) @?= Just DirLeft
  , testCase "should elect less visited cell" $
        let grid = ["#3#",
                    "2 4",
                    "#1#"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= Just DirUp
  , testCase "should elect less visited cell" $
        let grid = ["#2#",
                    "1 3",
                    "#4#"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= Just DirLeft
  , testCase "should elect less visited cell" $
        let grid = ["#1#",
                    "4 2",
                    "#3#"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= Just DirDown
  , testCase "should elect less visited cell" $
        let grid = ["#4#",
                    "3 1",
                    "#2#"] in
        (walkerBestDir (testWalker grid DirUp) grid) @?= Just DirRight
  ]

walkerCellsTests = testGroup "walkerCells"
  [ testCase "should create cells for all matrix elements" $
        let grid = ["123#",
                    "# # ",
                    "#2 #"] in
        matrixIterate (walkerCells $(testWalker grid DirUp))
                      cellVisits @?= [[1,2,3,4],
                                      [4,0,4,0],
                                      [4,2,0,4]]
   ,  testCase "should create cells for all matrix elements" $
        let grid = ["123#",
                    "# # ",
                    "#2 #"] in
        walkerPrintCell (testWalker grid DirUp) @?= ["123#",
                                                     "#0#0",
                                                     "#20#"]
  ]
