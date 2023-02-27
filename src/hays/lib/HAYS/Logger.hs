{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module HAYS.Logger
    ( Color (..)
    , ColorIntensity (..)
    , Level (..)
    , Logger
    , Record
    , Style (..)
    , debug
    , defaultNamespace
    , defaultTerminal
    , error'
    , formatted
    , fromDate
    , fromDays
    , fromHandle
    , fromHours
    , fromIO
    , fromIntegral
    , fromLevel
    , fromList
    , fromMicroseconds
    , fromMilliseconds
    , fromMinutes
    , fromMonth
    , fromNanoseconds
    , fromPicoseconds
    , fromSeconds
    , fromShow
    , fromTimeAndZone
    , fromTimeUTC
    , fromUUID
    , fromWeekday
    , fromWeeks
    , fromYears
    , info
    , namespace
    , plain
    , prefix
    , recordToText
    , removeFormatting
    , setBackground
    , setDefaultLevelFormatting
    , setForeground
    , setStyle
    , suffix
    , warn
    ) where

import qualified Data.List               as List
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text.IO
import qualified GHC.Exts
import           Lib.Time                (Time)
import qualified Lib.Time                as Time
import           Lib.Time.Date           (Date)
import qualified Lib.Time.Date           as Date
import qualified Lib.Time.Date.Formatter as Date.Formatter
import qualified Lib.Time.Formatter      as Time.Formatter
import qualified Lib.Time.Month          as Month
import qualified Lib.Time.Weekday        as Weekday
import           Lib.UUID                (UUID)
import qualified Lib.UUID                as UUID
import           Prelude                 hiding (fromIntegral)
import qualified System.IO               as IO

-- * Logger

newtype Logger
  = Logger (Level -> Record -> IO ())

-- ** Constructors

fromIO :: (Level -> Text -> IO ()) -> Logger
fromIO action =
  Logger $ \level -> action level . recordToText

fromHandle :: (Level -> IO.Handle) -> Logger
fromHandle getHandle =
  fromIO (Text.IO.hPutStrLn . getHandle)

defaultTerminal :: Logger
defaultTerminal =
  fromHandle $ \level ->
    case level of
      Error -> IO.stderr
      _     -> IO.stdout

-- ** Logging Functions

info :: Logger -> Record -> IO ()
info (Logger f) = f Info

warn :: Logger -> Record -> IO ()
warn (Logger f) = f Warn

error' :: Logger -> Record -> IO ()
error' (Logger f) = f Error

debug :: Logger -> Record -> IO ()
debug (Logger f) = f Debug

-- ** Modifiers

prefix :: (Level -> Record) -> Logger -> Logger
prefix getPrefixRecord (Logger f) =
  Logger $ \level record ->
    f level (getPrefixRecord level <> record)

namespace :: Record -> Record -> Record -> (Level -> [Record]) -> Logger -> Logger
namespace left right separator getNamespaceRecords =
  prefix (\level -> left <> fromList (List.intersperse separator (getNamespaceRecords level)) <> right)

defaultNamespace :: [Record] -> Logger -> Logger
defaultNamespace records =
  namespace left right' separator $ \level ->
    map (setDefaultLevelFormatting level) (fromLevel False level : records)
  where
    setGreyForeground = setForeground (Black Bright)
    left = setGreyForeground "["
    right' = setGreyForeground "] "
    separator = setGreyForeground ":"

suffix :: (Level -> Record) -> Logger -> Logger
suffix getSuffixRecord (Logger f) =
  Logger $ \level record ->
    f level (record <> getSuffixRecord level)

-- * Level

data Level = Info | Warn | Error | Debug deriving (Eq)

instance Show Level where
  show level =
    case level of
      Info  -> "info"
      Warn  -> "warn"
      Error -> "error"
      Debug -> "debug"

-- * Record

data Record
  = Plain !Text
  | Formatted
      { _foreground :: !Color
      , _background :: !Color
      , _style      :: !Style
      , _text       :: !Text
      }
  | Multiple !(NonEmpty Record)

-- | The 'IsString' instance of 'Record' constructs a 'plain' 'Record'
-- with no formatting.
instance GHC.Exts.IsString Record where
  fromString = plain . GHC.Exts.fromString

instance GHC.Exts.IsList Record where
  type Item Record = Record
  fromList = HAYS.Logger.fromList
  toList record =
    case record of
      Multiple records -> NonEmpty.toList records
      _                -> [record]


instance Semigroup Record where
  (<>) a b =
    case (a, b) of
      (Plain a', Plain b')       -> Plain $ a' <> b'
      (Multiple as, Multiple bs) -> Multiple $ as <> bs
      (Multiple as, _)           -> Multiple $ as <> [b]
      (_, Multiple bs)           -> Multiple $ NonEmpty.cons a bs
      _                          -> Multiple [a, b]

-- ** Constructors

plain :: Text -> Record
plain = Plain

formatted :: Color -> Color -> Style -> Text -> Record
formatted = Formatted

fromList :: [Record] -> Record
fromList records =
  case records of
    []     -> plain ""
    (s:ss) -> Multiple $ s :| ss

fromLevel :: Bool -> Level -> Record
fromLevel fixedWidth level =
  plain
    $ justify
    $ Text.pack
    $ show level
  where
    justify = if fixedWidth then Text.justifyLeft 5 ' ' else id

fromShow :: Show a => a -> Record
fromShow = plain . Text.pack . show

fromIntegral :: Integral a => a -> Record
fromIntegral = fromShow . toInteger

fromUUID :: UUID -> Record
fromUUID = plain . UUID.encodeBase64TextStrict

fromDate :: Date -> Record
fromDate =
  plain . Date.Formatter.format (Date.Formatter.iso8601Date Date.Formatter.Day)

fromTimeAndZone :: Time.Zone -> Time -> Record
fromTimeAndZone zone =
  plain . Time.Formatter.format (Time.Formatter.iso8601DateAndTime Time.Formatter.Day Time.Formatter.Millisecond) zone

fromTimeUTC :: Time -> Record
fromTimeUTC = fromTimeAndZone Time.utc

fromYears :: Time.Years -> Record
fromYears = (<> "y") . fromIntegral

fromMonth :: Time.Month -> Record
fromMonth = plain . Month.toText

fromWeeks :: Time.Weeks -> Record
fromWeeks = (<> "w") . fromIntegral

fromWeekday :: Time.Weekday -> Record
fromWeekday = plain . Weekday.toText

fromDays :: Time.Days -> Record
fromDays = (<> "d") . fromIntegral

fromHours :: Time.Hours -> Record
fromHours = (<> "h") . fromIntegral

fromMinutes :: Time.Minutes -> Record
fromMinutes = (<> "m") . fromIntegral

fromSeconds :: Time.Seconds -> Record
fromSeconds = (<> "s") . fromIntegral

fromMilliseconds :: Time.Milliseconds -> Record
fromMilliseconds = (<> "ms") . fromIntegral

fromMicroseconds :: Time.Microseconds -> Record
fromMicroseconds = (<> "µs") . fromIntegral

fromNanoseconds :: Time.Nanoseconds -> Record
fromNanoseconds = (<> "ns") . fromIntegral

fromPicoseconds :: Time.Picoseconds -> Record
fromPicoseconds = (<> "ps") . fromIntegral

-- ** Converters

recordToText :: Record -> Text
recordToText record =
  case record of
    Plain text -> text
    Formatted {..} ->
      mconcat
        [ colorToSequence Foreground _foreground
        , colorToSequence Background _background
        , styleToSequence _style
        , _text
        , resetSequence
        ]
    Multiple records ->
      foldr ((<>) . recordToText) "" records


-- ** Setters

setForeground :: Color -> Record -> Record
setForeground color record =
  case record of
    Plain t ->
      formatted color Default Regular t
    Formatted {..} ->
      let _foreground = color in Formatted {..}
    Multiple records ->
      Multiple $ fmap (setForeground color) records

setBackground :: Color -> Record -> Record
setBackground color record =
  case record of
    Plain t ->
      formatted Default color Regular t
    Formatted {..} ->
      let _background = color in Formatted {..}
    Multiple records ->
      Multiple $ fmap (setBackground color) records

setStyle :: Style -> Record -> Record
setStyle style record =
  case record of
    Plain t ->
      formatted Default Default style t
    Formatted {..} ->
      let _style = style in Formatted {..}
    Multiple records ->
      Multiple $ fmap (setStyle style) records

setDefaultLevelFormatting :: Level -> Record -> Record
setDefaultLevelFormatting level =
  setStyle Regular
    . setBackground Default
    . setForeground
        (case level of
          Info  -> Cyan Normal
          Warn  -> Yellow Normal
          Error -> Red Normal
          Debug -> Blue Normal)

removeFormatting :: Record -> Record
removeFormatting record =
  case record of
    Plain _          -> record
    Formatted {..}   -> Plain _text
    Multiple records -> Multiple $ fmap removeFormatting records

-- * Color

data Color
  = Default
  | Black ColorIntensity
  | Red ColorIntensity
  | Green ColorIntensity
  | Yellow ColorIntensity
  | Blue ColorIntensity
  | Magenta ColorIntensity
  | Cyan ColorIntensity
  | White ColorIntensity

-- ** Converters

colorToSequence :: ColorTarget -> Color -> Text
colorToSequence target color =
  let
    codePrefix intensity=
      case (intensity, target) of
        (Normal, Foreground) -> "3"
        (Normal, Background) -> "4"
        (Bright, Foreground) -> "9"
        (Bright, Background) -> "10"

    makeSequence' intensity n =
      makeSequence $ codePrefix intensity <> n
  in
  case color of
    Default           -> makeSequence' Normal "9"
    Black intensity   -> makeSequence' intensity "0"
    Red intensity     -> makeSequence' intensity "1"
    Green intensity   -> makeSequence' intensity "2"
    Yellow intensity  -> makeSequence' intensity "3"
    Blue intensity    -> makeSequence' intensity "4"
    Magenta intensity -> makeSequence' intensity "5"
    Cyan intensity    -> makeSequence' intensity "6"
    White intensity   -> makeSequence' intensity "7"

-- ** Intensity

data ColorIntensity = Normal | Bright

-- ** ColorTarget

data ColorTarget = Foreground | Background

-- * Style

data Style = Regular | Bold | Dim | Italic | Underline | Blink

-- ** Converters

styleToSequence :: Style -> Text
styleToSequence style =
  case style of
    Regular   -> ""
    Bold      -> makeSequence "1"
    Dim       -> makeSequence "2"
    Italic    -> makeSequence "3"
    Underline -> makeSequence "4"
    Blink     -> makeSequence "5"

-- * Helpers

makeSequence :: Text -> Text
makeSequence code = Text.concat ["\ESC[", code, "m"]

resetSequence :: Text
resetSequence = makeSequence "0"
