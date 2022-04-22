{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Provides the 'TimeRep' type and related functions for representing
-- time.
module Data.TimeRep
  ( -- * Type
    TimeRep (..),

    -- * Conversions
    toSeconds,
    fromSeconds,
    fromString,

    -- * Formatting
    formatTimeRep,
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
-- 'fromString' will parse this into 'TimeRep', and then the application can
-- either convert this into seconds or keep as a 'TimeRep' as needed.
--
-- @since 0.1
data TimeRep = MkTimeRep
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
instance Ord TimeRep where
  x <= y = toSeconds x <= toSeconds y

-- @since 0.1
instance Read TimeRep where
  readPrec = readRecord +++ readSeconds +++ readTimeStr

-- @since 0.1
instance ASemigroup TimeRep where
  MkTimeRep d h m s .+. MkTimeRep d' h' m' s' =
    MkTimeRep (d + d') (h + h') (m + m') (s + s')

-- @since 0.1
instance AMonoid TimeRep where
  zero = MkTimeRep 0 0 0 0

-- | Transforms a 'TimeRep' into 'Natural' seconds.
--
-- @since 0.1
toSeconds :: TimeRep -> Natural
toSeconds (MkTimeRep d h m s) =
  d * secondsInDay
    + h * secondsInHour
    + m * secondsInMinute
    + s

-- | Transforms 'Natural' seconds into a 'TimeRep'.
--
-- @since 0.1
fromSeconds :: Natural -> TimeRep
fromSeconds seconds' = MkTimeRep d h m s
  where
    (d, daysRem) = seconds' `quotRem` secondsInDay
    (h, hoursRem) = daysRem `quotRem` secondsInHour
    (m, s) = hoursRem `quotRem` secondsInMinute

-- | Converts a 'String' into a 'TimeRep'.
--
-- @since 0.1
fromString :: String -> Either String TimeRep
fromString str = case RPC.readPrec_to_S read' RPC.minPrec str of
  [(y, "")] -> Right y
  _ -> Left $ "Could not read TimeRep from: " <> str
  where
    read' = readTimeStr +++ readSeconds <* RPC.lift RP.skipSpaces

-- | Formats a 'TimeRep' to 'String'.
--
-- @since 0.1
formatTimeRep :: TimeRep -> String
formatTimeRep (MkTimeRep 0 0 0 0) = "0 seconds"
formatTimeRep (MkTimeRep d h m s) = L.intercalate ", " vals
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
formatTime = formatTimeRep . fromSeconds

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

readRecord :: ReadPrec TimeRep
readRecord = GRead.parens $
  RPC.prec 11 $ do
    GRead.expectP (Ident "MkTimeRep")
    GRead.expectP (Punc "{")
    d <- GRead.readField "days" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc ",")
    h <- GRead.readField "hours" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc ",")
    m <- GRead.readField "minutes" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc ",")
    s <- GRead.readField "seconds" (RPC.reset GRead.readPrec)
    GRead.expectP (Punc "}")
    pure $ MkTimeRep d h m s

readSeconds :: ReadPrec TimeRep
readSeconds = fromSeconds <$> readPrec

readTimeStr :: ReadPrec TimeRep
readTimeStr =
  MkTimeRep
    <$> readTimeOrZero 'd'
    <*> readTimeOrZero 'h'
    <*> readTimeOrZero 'm'
    <*> readTimeOrZero 's'

readTimeOrZero :: Char -> ReadPrec Natural
readTimeOrZero c =
  readTimeWithUnit c <|> pure 0

readTimeWithUnit :: Char -> ReadPrec Natural
readTimeWithUnit c = readPrec <* RPC.lift (RP.char c)
