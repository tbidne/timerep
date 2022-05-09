{-# LANGUAGE UndecidableInstances #-}

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
    diffRelativeTime,

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
import Data.Kind (Type)
import Data.List qualified as L
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Read qualified as GRead
import Numeric.Algebra
  ( AMonoid (..),
    ASemigroup (..),
    MSemiSpace (..),
    MSpace (..),
    NonZero (..),
    Semimodule,
    SemivectorSpace,
  )
import Optics.Core (A_Lens, An_Iso, LabelOptic (..), iso, lens)
import Text.ParserCombinators.ReadP qualified as RP
import Text.ParserCombinators.ReadPrec (ReadPrec, (+++))
import Text.ParserCombinators.ReadPrec qualified as RPC
import Text.Read (Read (..))
import Text.Read.Lex (Lexeme (..))

-- $setup
-- >>> let sleepSeconds _ = pure ()

-- | Represents a relative time with second precision. This is primarily
-- intended to be used when an application defines some numeric value in
-- terms of seconds (e.g. timeout, poll interval) but the values may be
-- large enough that literal seconds are inconvenient / error prone.
--
-- For example, suppose an application takes an argument representing a
-- timeout. Asking for a natural number representing seconds is reasonable,
-- but it has low UX if there is any chance this timeout could be somewhat
-- large e.g. over an hour.
--
-- 'RelativeTime' makes this more convenient by allowing one to supply larger
-- values in terms of days, hours, minutes, and seconds, and includes
-- conversion functions. For instance, if we want to put a thread to sleep
-- for an hour, we could do:
--
-- >>> :{
-- sleep1Hour :: IO ()
-- sleep1Hour = do
--   let rt = zero { hours = 1 }
--   -- sleepSeconds :: Natural -> IO ()
--   sleepSeconds $ toSeconds rt
-- :}
--
-- Furthermore, we provide convenient string parsing (for e.g. user config)
-- in the form of "time strings" like "1s2h3m4s". 'fromString' will parse
-- this into 'RelativeTime', and then the application can either convert this
-- into seconds or keep as a 'RelativeTime' as needed.
--
-- ==== __Instance Details__
--
-- * 'Eq'/'Ord': Terms are converted first to seconds then compared i.e. we
--   declare an equivalence class in terms of the "total time" represented.
-- * 'Read': Parses the same strings as 'fromString'. Additionally, we also
--   parse the output of 'Show' i.e. the derived instance.
-- * Optics: In addition to the obvious lenses, we also provide an 'Iso'
--   between 'RelativeTime' and 'Natural' seconds.
--
-- >>> read @RelativeTime "MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}"
-- MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}
--
-- @since 0.1
type RelativeTime :: Type
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
instance (k ~ A_Lens) => LabelOptic "days" k RelativeTime RelativeTime Natural Natural where
  labelOptic = lens days (\rt d -> rt {days = d})

-- | @since 0.1
instance (k ~ A_Lens) => LabelOptic "hours" k RelativeTime RelativeTime Natural Natural where
  labelOptic = lens hours (\rt d -> rt {hours = d})

-- | @since 0.1
instance (k ~ A_Lens) => LabelOptic "minutes" k RelativeTime RelativeTime Natural Natural where
  labelOptic = lens minutes (\rt d -> rt {minutes = d})

-- | @since 0.1
instance (k ~ A_Lens) => LabelOptic "seconds" k RelativeTime RelativeTime Natural Natural where
  labelOptic = lens seconds (\rt d -> rt {seconds = d})

-- | @since 0.1
instance (k ~ An_Iso) => LabelOptic "secondsIso" k RelativeTime RelativeTime Natural Natural where
  labelOptic = iso toSeconds fromSeconds

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
-- Operations on 'RelativeTime'. In addition to the following operations, we
-- also have instances from @algebra-simple@:
--
-- >>> -- Addition
-- >>> import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
-- >>> let t1 = MkRelativeTime 1 2 3 4
-- >>> let t2 = MkRelativeTime 2 3 4 5
-- >>> t1 .+. t2
-- MkRelativeTime {days = 3, hours = 5, minutes = 7, seconds = 9}
--
-- >>> t1 .+. zero
-- MkRelativeTime {days = 1, hours = 2, minutes = 3, seconds = 4}
--
-- >>> -- Scalar multiplication
-- >>> import Numeric.Algebra.Space.MSemiSpace (MSemiSpace ((.*)))
-- >>> t1 .* 2
-- MkRelativeTime {days = 2, hours = 4, minutes = 6, seconds = 8}
--
-- >>> -- Scalar division
-- >>> import Numeric.Algebra.Space.MSpace (MSpace ((.%)))
-- >>> import Numeric.Data.NonZero (unsafeNonZero)
-- >>> t1 .% unsafeNonZero 2
-- MkRelativeTime {days = 0, hours = 1, minutes = 1, seconds = 2}
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

-- | Returns the absolute difference between two relative times.
-- Despite 'Natural' subtraction being partial, 'diffRelativeTime' is total.
--
-- ==== __Examples__
--
-- >>> diffRelativeTime (MkRelativeTime 2 0 0 4) (MkRelativeTime 1 0 0 2)
-- MkRelativeTime {days = 1, hours = 0, minutes = 0, seconds = 2}
--
-- >>> diffRelativeTime (MkRelativeTime 3 6 2 41) (MkRelativeTime 8 2 4 1)
-- MkRelativeTime {days = 4, hours = 20, minutes = 1, seconds = 20}
--
-- @since 0.1
diffRelativeTime :: RelativeTime -> RelativeTime -> RelativeTime
diffRelativeTime r1 r2 = fromSeconds $ toSeconds r1 `diffNat` toSeconds r2
  where
    diffNat n1 n2
      | n1 >= n2 = n1 - n2
      | otherwise = n2 - n1

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
-- Left "Could not read RelativeTime from: "
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
readTimeStr = do
  s <- RPC.look
  if null s
    then RPC.pfail
    else
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
