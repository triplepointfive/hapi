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
        cellDirections (cellVisit emptyCell DirUp) @?= [DirUp]
  , testCase "mark visible if visit all directions" $
        cellVisited (mapCall cellVisit emptyCell dirs) @?= True
  ]
