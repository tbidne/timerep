module Unit.Data.Time.Relative
  ( props,
  )
where

import Data.List (sort)
import Data.Time.Relative (RelativeTime (..))
import Data.Time.Relative qualified as Relative
import GHC.Natural (Natural)
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra (AMonoid (..), ASemigroup (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyH
import Text.Read qualified as TR
import Unit.MaxRuns (MaxRuns (..))

props :: TestTree
props =
  Tasty.testGroup
    "Properties"
    [ testToFromId,
      testEq,
      testOrd,
      testNormalize,
      testDiff,
      testReadShowId,
      testRead,
      testFromString
    ]

testToFromId :: TestTree
testToFromId = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "toSeconds . fromSeconds = id" "testToFromId" $
    H.withTests limit $
      H.property $ do
        seconds' <- H.forAll gnat
        seconds' === f seconds'
  where
    f = Relative.toSeconds . Relative.fromSeconds

testEq :: TestTree
testEq = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "Eq is determined by seconds" "testEq" $
    H.withTests limit $
      H.property $ do
        r1 <- H.forAll grelativeTime
        r2 <- H.forAll grelativeTime
        let r1' = Relative.toSeconds r1
        let r2' = Relative.toSeconds r2
        H.annotateShow r1'
        H.annotateShow r2'
        (r1 == r2) === (r1' == r2')
        r1 === Relative.normalize r1

testOrd :: TestTree
testOrd = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "Ord is determined by seconds" "testOrd" $
    H.withTests limit $
      H.property $ do
        r1 <- H.forAll grelativeTime
        r2 <- H.forAll grelativeTime
        let r1' = Relative.toSeconds r1
        let r2' = Relative.toSeconds r2
        H.annotateShow r1'
        H.annotateShow r2'
        (r1 <= r2) === (r1' <= r2')

testNormalize :: TestTree
testNormalize = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "normalize rt reduces all fields" "testNormalize" $
    H.withTests limit $
      H.property $ do
        rt <- H.forAll grelativeTime
        let rt'@(MkRelativeTime _ h m s) = Relative.normalize rt
        H.annotateShow rt'
        H.assert $ h < 24
        H.assert $ m < 60
        H.assert $ s < 60

testDiff :: TestTree
testDiff = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "diffRelativeTime additive algebra" "testDiff" $
    H.withTests limit $
      H.property $ do
        r1 <- H.forAll grelativeTime
        r2 <- H.forAll grelativeTime
        let result = Relative.diffRelativeTime r1 r2
        H.annotateShow result
        if
            | r1 > r2 -> r1 === r2 .+. result
            | r2 > r1 -> r2 === r1 .+. result
            | otherwise -> zero === result

testReadShowId :: TestTree
testReadShowId = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "read . show = id" "testReadShowId" $
    H.withTests limit $
      H.property $ do
        rt <- H.forAll grelativeTime
        let rt' = f rt
        H.annotateShow rt'
        rt === rt'
  where
    f = read . show

testRead :: TestTree
testRead = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "Reads strings" "testRead" $
    H.withTests limit $
      H.property $ do
        s <- H.forAll greadable
        let result = TR.readEither @RelativeTime s
        case result of
          Left err -> do
            H.annotate $ "Failed to read str. Received: " <> err
            H.failure
          Right _ -> pure ()

testFromString :: TestTree
testFromString = Tasty.askOption $ \(MkMaxRuns limit) ->
  TastyH.testPropertyNamed "fromString with Time String and Seconds succeeds" "testFromString" $
    H.withTests limit $
      H.property $ do
        s <- H.forAll gsecOrTimeStr
        let result = Relative.fromString s
        case result of
          Left err -> do
            H.annotate $ "Failed to parse str. Received: " <> err
            H.failure
          Right _ -> pure ()

gnat :: Gen Natural
gnat = HG.integral $ HR.exponential 0 1_000_000

grelativeTime :: Gen RelativeTime
grelativeTime =
  MkRelativeTime
    <$> gnat
    <*> gnat
    <*> gnat
    <*> gnat

greadable :: Gen String
greadable = HG.choice [gnatStr, grelativeTimeStr, gtimeStr]

gsecOrTimeStr :: Gen String
gsecOrTimeStr = HG.choice [gnatStr, gtimeStr]

gnatStr :: Gen String
gnatStr = show <$> gnat

grelativeTimeStr :: Gen String
grelativeTimeStr = show <$> grelativeTime

gtimeStr :: Gen String
gtimeStr = do
  toTake <- HG.integral (HR.linear 1 4)
  fullStrShuffled <- gAllUnitsShuffled
  let someUnits = take toTake fullStrShuffled
  pure (unitToStr =<< sort someUnits)

data TimeStrUnit
  = Days Natural
  | Hours Natural
  | Minutes Natural
  | Seconds Natural
  deriving stock (Eq, Show)

instance Ord TimeStrUnit where
  Days _ `compare` Days _ = EQ
  Days _ `compare` _ = LT
  _ `compare` Days _ = GT
  Hours _ `compare` Hours _ = EQ
  Hours _ `compare` _ = LT
  _ `compare` Hours _ = GT
  Minutes _ `compare` Minutes _ = EQ
  Minutes _ `compare` _ = LT
  _ `compare` Minutes _ = GT
  Seconds _ `compare` Seconds _ = EQ

gAllUnitsShuffled :: Gen [TimeStrUnit]
gAllUnitsShuffled = do
  d <- gnat
  h <- gnat
  m <- gnat
  s <- gnat
  HG.shuffle [Days d, Hours h, Minutes m, Seconds s]

unitToStr :: TimeStrUnit -> String
unitToStr (Days d) = show d <> "d"
unitToStr (Hours h) = show h <> "h"
unitToStr (Minutes m) = show m <> "m"
unitToStr (Seconds s) = show s <> "s"
