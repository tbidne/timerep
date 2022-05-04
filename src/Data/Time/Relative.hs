-- | Provides the 'RelativeTime' type and related functions for representing
-- time.
--
-- @since 0.1
module Data.Time.Relative
  ( -- * Type
    RelativeTime (..),

    -- * Operations
    -- $operations
    normalize,

    -- * Conversions
    toSeconds,
    fromSeconds,
    fromString,

    -- * Formatting
    formatRelativeTime,
    formatSeconds,
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
import Numeric.Algebra
  ( AMonoid (..),
    ASemigroup (..),
    MSemiSpace (..),
    SemivectorSpace,
    MSpace (..),
    NonZero (..),
    Semimodule,
  )
import Text.ParserCombinators.ReadP qualified as RP
import Text.ParserCombinators.ReadPrec (ReadPrec, (+++))
import Text.ParserCombinators.ReadPrec qualified as RPC
import Text.Read (Read (..))
import Text.Read.Lex (Lexeme (..))

-- | Represents a relative time with second precision. This is primarily
-- intended to be used with user supplied numeric values for convenience.
--
-- For example, suppose an application takes an argument representing a
-- timeout. Asking for a natural number representing seconds is reasonable,
-- but it has low UX if there is any chance this timeout could be somewhat
-- large e.g. over an hour.
--
-- Instead, we can allow the user to supply a "time string" like "1d2h3m4s".
-- 'fromString' will parse this into 'RelativeTime', and then the application can
-- either convert this into seconds or keep as a 'RelativeTime' as needed.
--
-- ==== __Instances__
--
-- * 'Eq'/'Ord': Terms are converted first to seconds then compared i.e. we
--   declare an equivalence class in terms of the "total time" represented.
-- * 'Read': Parses the same strings as 'fromString'. Additionally, we also
--   parse the output of 'Show' i.e. the derived instance.
--
-- >>> read @RelativeTime "MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}"
-- MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}
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

-- | @since 0.1
instance Ord RelativeTime where
  x <= y = toSeconds x <= toSeconds y

-- | @since 0.1
instance Read RelativeTime where
  readPrec = readRecord +++ readSeconds +++ readTimeStr

-- | @since 0.1
instance ASemigroup RelativeTime where
  MkRelativeTime d h m s .+. MkRelativeTime d' h' m' s' =
    normalize $ MkRelativeTime (d + d') (h + h') (m + m') (s + s')

-- | @since 0.1
instance AMonoid RelativeTime where
  zero = MkRelativeTime 0 0 0 0

-- | @since 0.1
instance MSemiSpace RelativeTime Natural where
  MkRelativeTime d h m s .* k =
    normalize $ MkRelativeTime (d * k) (h * k) (m * k) (s * k)

-- | @since 0.1
instance MSpace RelativeTime Natural where
  MkRelativeTime d h m s .% MkNonZero k =
    normalize $ MkRelativeTime (d `div` k) (h `div` k) (m `div` k) (s `div` k)

-- | @since 0.1
instance Semimodule RelativeTime Natural

-- | @since 0.1
instance SemivectorSpace RelativeTime Natural

-- $operations
-- Operations on 'RelativeTime'. In addition to the unary 'normalize', we also
-- have instances from @algebra-simple@:
--
-- >>> let t1 = MkRelativeTime 1 2 3 4
-- >>> let t2 = MkRelativeTime 2 3 4 5
-- >>> t1 .+. t2
-- MkRelativeTime {days = 3, hours = 5, minutes = 7, seconds = 9}
--
-- >>> t1 .+. zero
-- MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}
--
-- >>> t1 .* 2
-- MkRelativeTime {days = 2, hours = 4, minutes = 6, seconds = 8}
--
-- These operations are 'normalize'd.

-- | Transforms the 'RelativeTime' into "canonical" form i.e. if we can reduce
-- some unit (e.g. 75 seconds), then we do.
--
-- ==== __Examples__
--
-- >>> normalize $ MkRelativeTime 0 0 0 75
-- MkRelativeTime {days = 0, hours = 0, minutes = 1, seconds = 15}
--
-- >>> normalize $ MkRelativeTime 2 23 59 61
-- MkRelativeTime {days = 3, hours = 0, minutes = 0, seconds = 1}
--
-- @since 0.1
normalize :: RelativeTime -> RelativeTime
normalize = fromSeconds . toSeconds

-- | Transforms a 'RelativeTime' into 'Natural' seconds.
--
-- ==== __Examples__
--
-- >>> toSeconds $ MkRelativeTime 0 1 1 1
-- 3661
--
-- >>> toSeconds $ MkRelativeTime 1 2 3 4
-- 93784
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
-- ==== __Examples__
--
-- >>> fromSeconds 3661
-- MkRelativeTime {days = 0, hours = 1, minutes = 1, seconds = 1}
--
-- >>> fromSeconds 93784
-- MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}
--
-- @since 0.1
fromSeconds :: Natural -> RelativeTime
fromSeconds seconds' = MkRelativeTime d h m s
  where
    (d, daysRem) = seconds' `quotRem` secondsInDay
    (h, hoursRem) = daysRem `quotRem` secondsInHour
    (m, s) = hoursRem `quotRem` secondsInMinute

-- | Converts a string into a 'RelativeTime'. Converts either a
-- "time string" e.g. "1d2h3m4s" or numeric literal (interpreted
-- as seconds).
--
-- ==== __Examples__
--
-- >>> fromString "1d2h3m4s"
-- Right (MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4})
--
-- >>> fromString "1h1s"
-- Right (MkRelativeTime {days = 0, hours = 1, minutes = 0, seconds = 1})
--
-- >>> fromString "0h15s"
-- Right (MkRelativeTime {days = 0, hours = 0, minutes = 0, seconds = 15})
--
-- >>> fromString "3601"
-- Right (MkRelativeTime {days = 0, hours = 1, minutes = 0, seconds = 1})
--
-- >>> fromString ""
-- Right (MkRelativeTime {days = 0, hours = 0, minutes = 0, seconds = 0})
--
-- >>> fromString "1s1h"
-- Left "Could not read RelativeTime from: 1s1h"
--
-- >>> fromString "cat"
-- Left "Could not read RelativeTime from: cat"
--
-- @since 0.1
fromString :: String -> Either String RelativeTime
fromString str =
  case [x | (x, "") <- RPC.readPrec_to_S read' RPC.minPrec str] of
    [y] -> Right y
    _ -> Left $ "Could not read RelativeTime from: " <> str
  where
    read' = readTimeStr +++ readSeconds <* RPC.lift RP.skipSpaces

-- | Formats a 'RelativeTime' to string.
--
-- ==== __Examples__
--
-- >>> formatRelativeTime $ MkRelativeTime 1 2 0 3
-- "1 day, 2 hours, 3 seconds"
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

-- | For \(n \ge 0\) seconds, returns a string description of the days, hours,
-- minutes and seconds.
--
-- ==== __Examples__
--
-- >>> formatSeconds 3623
-- "1 hour, 23 seconds"
--
--
-- @since 0.1
formatSeconds :: Natural -> String
formatSeconds = formatRelativeTime . fromSeconds

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
