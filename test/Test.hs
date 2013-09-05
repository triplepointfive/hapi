module Main where

import qualified Test.App.Cell
import qualified Test.App.Stuff
import qualified Test.App.Matrix
import qualified Test.App.Walker
import qualified Test.App.Logger

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ Test.App.Cell.tests
    , Test.App.Stuff.tests
    , Test.App.Matrix.tests
    , Test.App.Walker.tests
    , Test.App.Logger.tests
    ]

