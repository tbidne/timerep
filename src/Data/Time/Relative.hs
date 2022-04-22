{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Provides the 'RelativeTime' type and related functions for representing
-- time.
module Data.Time.Relative
  ( -- * Type
    RelativeTime (..),

    -- * Conversions
    toSeconds,
    fromSeconds,
    fromString,

    -- * Formatting
    formatRelativeTime,
    formatTime,
  )
where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.List qualified as L
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Read qualified as GRead
import Numeric.Algebra (AMonoid (..), ASemigroup (..))
import Text.ParserCombinators.ReadP qualified as RP
import Text.ParserCombinators.ReadPrec (ReadPrec, (+++))
import Text.ParserCombinators.ReadPrec qualified as RPC
import Text.Read (Read (..))
import Text.Read.Lex (Lexeme (..))

-- | Represents a relative time with second granularity. This is primarily
-- intended to be used with user supplied numeric values for convenience.
--
-- For example, suppose an application takes an argument representing a
-- timeout. Asking for a natural number representing seconds is reasonable,
-- but it has low UX if there is any chance this timeout could be somewhat
-- large e.g. over an hour.
--
-- Instead, we can allow the user supply a "time string" like "1d2h3m4s".
-- 'fromString' will parse this into 'RelativeTime', and then the application can
-- either convert this into seconds or keep as a 'RelativeTime' as needed.
--
-- @since 0.1
data RelativeTime = MkRelativeTime
  { -- | @since 0.1
    days :: !Natural,
    -- | @since 0.1
    hours :: !Natural,
    -- | @since 0.1
    minutes :: !Natural,
    -- | @since 0.1
    seconds :: !Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Data,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- @since 0.1
instance Ord RelativeTime where
  x <= y = toSeconds x <= toSeconds y

-- @since 0.1
instance Read RelativeTime where
  readPrec = readRecord +++ readSeconds +++ readTimeStr

-- @since 0.1
instance ASemigroup RelativeTime where
  MkRelativeTime d h m s .+. MkRelativeTime d' h' m' s' =
    MkRelativeTime (d + d') (h + h') (m + m') (s + s')

-- @since 0.1
instance AMonoid RelativeTime where
  zero = MkRelativeTime 0 0 0 0

-- | Transforms a 'RelativeTime' into 'Natural' seconds.
--
-- @since 0.1
toSeconds :: RelativeTime -> Natural
toSeconds (MkRelativeTime d h m s) =
  d * secondsInDay
    + h * secondsInHour
    + m * secondsInMinute
    + s

-- | Transforms 'Natural' seconds into a 'RelativeTime'.
--
-- @since 0.1
fromSeconds :: Natural -> RelativeTime
fromSeconds seconds' = MkRelativeTime d h m s
  where
    (d, daysRem) = seconds' `quotRem` secondsInDay
    (h, hoursRem) = daysRem `quotRem` secondsInHour
    (m, s) = hoursRem `quotRem` secondsInMinute

-- | Converts a 'String' into a 'RelativeTime'.
--
-- @since 0.1
fromString :: String -> Either String RelativeTime
fromString str = case RPC.readPrec_to_S read' RPC.minPrec str of
  [(y, "")] -> Right y
  _ -> Left $ "Could not read RelativeTime from: " <> str
  where
    read' = readTimeStr +++ readSeconds <* RPC.lift RP.skipSpaces

-- | Formats a 'RelativeTime' to 'String'.
--
-- @since 0.1
formatRelativeTime :: RelativeTime -> String
formatRelativeTime (MkRelativeTime 0 0 0 0) = "0 seconds"
formatRelativeTime (MkRelativeTime d h m s) = L.intercalate ", " vals
  where
    f acc (n, units)
      | n == 0 = acc
      | otherwise = pluralize n units : acc
    vals = foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]

-- | For \(n \ge 0\) seconds, returns a 'String' description of the days, hours,
-- minutes and seconds.
--
-- @since 0.1
formatTime :: Natural -> String
formatTime = formatRelativeTime . fromSeconds

pluralize :: Natural -> String -> String
pluralize n txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    valUnit = show n <> txt

secondsInDay :: Natural
secondsInDay = 86_400

secondsInHour :: Natural
secondsInHour = 3_600

secondsInMinute :: Natural
secondsInMinute = 60

readRecord :: ReadPrec RelativeTime
readRecord = GRead.parens $
  RPC.prec 11 $ do
    GRead.expectP (Ident "MkRelativeTime")
    GRead.expectP (Punc "{")
    d <- GRead.readField "days" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc ",")
    h <- GRead.readField "hours" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc ",")
    m <- GRead.readField "minutes" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc ",")
    s <- GRead.readField "seconds" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc "}")
    pure $ MkRelativeTime d h m s

readSeconds :: ReadPrec RelativeTime
readSeconds = fromSeconds <$> readPrec

readTimeStr :: ReadPrec RelativeTime
readTimeStr =
  MkRelativeTime
    <$> readTimeOrZero 'd'
    <*> readTimeOrZero 'h'
    <*> readTimeOrZero 'm'
    <*> readTimeOrZero 's'

readTimeOrZero :: Char -> ReadPrec Natural
readTimeOrZero c =
  readTimeWithUnit c <|> pure 0

readTimeWithUnit :: Char -> ReadPrec Natural
readTimeWithUnit c = readPrec <* RPC.lift (RP.char c)
