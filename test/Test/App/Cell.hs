module Test.App.Cell (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import App.Cell
import App.Direction
import App.Stuff

tests :: TestTree
tests = testGroup "Cell"
    [ cellVisitTests
    ]

cellVisitTests = testGroup "cellVisit"
  [ testCase "move up from cell" $
        cellVisits (cellVisit emptyCell) @?= 1
  , testCase "mark visible if visit all directions" $
        cellVisited (applyNTimes cellVisit emptyCell $ length dirs) @?= True
  ]
