-- | Runs unit tests.
module Main (main) where

import Control.Monad qualified as M
import Data.Proxy (Proxy (..))
import System.Environment qualified as Env
import System.Exit qualified as SysEx
import Test.Tasty qualified as Tasty
import Test.Tasty.Options (OptionDescription (..))
import Text.Read qualified as TR
import Unit.Data.Time.Relative qualified as Relative
import Unit.MaxRuns (MaxRuns (..))

-- | Entry point for unit tests.
main :: IO ()
main = do
  maxRuns <-
    Env.lookupEnv "MAX_RUNS" >>= \case
      Nothing -> pure 100
      Just mr -> case parseMaxRuns mr of
        Nothing -> SysEx.die $ "*** MAX_RUNS is not a non-negative integer: " <> mr
        Just x -> pure $ fromIntegral @Int x

  let maxRunProps = Tasty.localOption (MkMaxRuns maxRuns) Relative.props

  Tasty.defaultMainWithIngredients ingredients $
    Tasty.testGroup "Unit tests" [maxRunProps]
  where
    parseMaxRuns = M.mfilter (> 0) . TR.readMaybe
    ingredients = Tasty.includingOptions [Option @MaxRuns Proxy] : Tasty.defaultIngredients
