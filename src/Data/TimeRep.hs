{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides the 'TimeRep' type and related functions for representing
-- time.
module Data.TimeRep
  ( -- * Type
    TimeRep (..),

    -- * Conversions
    toSeconds,
    fromSeconds,

    -- * Formatting
    formatTimeRep,
    formatTime,
  )
where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Read qualified as GRead
import Text.ParserCombinators.ReadP qualified as RP
import Text.ParserCombinators.ReadPrec (ReadPrec, (+++))
import Text.ParserCombinators.ReadPrec qualified as RPC
import Text.Read (Read (..))
import Text.Read.Lex (Lexeme (..))

-- | Represents a relative time.
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
    ( -- @since 0.1
      NFData
    )

-- @since 0.1
instance Ord TimeRep where
  x <= y = toSeconds x <= toSeconds y

-- @since 0.1
instance Read TimeRep where
  readPrec = readRecord +++ readSeconds +++ readTimeStr

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

-- | Formats a 'TimeRep' to 'Text'.
--
-- @since 0.1
formatTimeRep :: TimeRep -> Text
formatTimeRep (MkTimeRep 0 0 0 0) = "0 seconds"
formatTimeRep (MkTimeRep d h m s) = T.intercalate ", " vals
  where
    f acc (n, units)
      | n == 0 = acc
      | otherwise = pluralize n units : acc
    vals = foldl' f [] [(s, " second"), (m, " minute"), (h, " hour"), (d, " day")]

-- | For \(n \ge 0\) seconds, returns a 'Text' description of the days, hours,
-- minutes and seconds.
--
-- @since 0.1
formatTime :: Natural -> Text
formatTime = formatTimeRep . fromSeconds

pluralize :: Natural -> Text -> Text
pluralize n txt
  | n == 1 = valUnit
  | otherwise = valUnit <> "s"
  where
    valUnit = T.pack (show n) <> txt

-- | Seconds in a day: 86,400
--
-- @since 0.1
secondsInDay :: Natural
secondsInDay = 86_400

-- | Seconds in an hour: 3,600
--
-- @since 0.1
secondsInHour :: Natural
secondsInHour = 3_600

-- | Seconds in a minute: 60
--
-- @since 0.1
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
