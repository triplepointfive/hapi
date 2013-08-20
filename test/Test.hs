module Main where

import qualified Test.App.Cell
import qualified Test.App.Stuff
import qualified Test.App.Matrix
-- import qualified Test.App.Direction

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ Test.App.Cell.tests
    , Test.App.Stuff.tests
    , Test.App.Matrix.tests
    --, Test.App..tests
    ]

