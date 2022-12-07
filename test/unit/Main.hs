-- | Runs unit tests.
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Data.Time.Relative qualified as Relative

-- | Entry point for unit tests.
main :: IO ()
main = Tasty.defaultMain Relative.tests
