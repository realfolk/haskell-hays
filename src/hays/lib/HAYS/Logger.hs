{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HAYS.Logger
    ( Color (..)
    , ColorIntensity (..)
    , Level (..)
    , Logger
    , Section
    , Style (..)
    , debug
    , defaultNamespace
    , error'
    , fromDate
    , fromDays
    , fromHours
    , fromIntegral
    , fromLevel
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
    , sectionToText
    , setBackground
    , setDefaultLevelFormatting
    , setForeground
    , setStyle
    , styled
    , suffix
    , terminal
    , warn
    ) where

import qualified Data.List               as List
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
  = Logger (Level -> [Section] -> IO ())

-- ** Constructors

terminal :: Logger
terminal =
  Logger $ \level sections ->
    Text.IO.hPutStrLn (handle level) $ sectionsToText sections
  where
    sectionsToText :: [Section] -> Text
    sectionsToText = foldr ((<>) . sectionToText) ""
    handle level =
      case level of
        Error -> IO.stderr
        _     -> IO.stdout

-- ** Logging Functions

info :: Logger -> [Section] -> IO ()
info (Logger f) = f Info

warn :: Logger -> [Section] -> IO ()
warn (Logger f) = f Warn

error' :: Logger -> [Section] -> IO ()
error' (Logger f) = f Error

debug :: Logger -> [Section] -> IO ()
debug (Logger f) = f Debug

-- ** Modifiers

prefix :: (Level -> [Section]) -> Logger -> Logger
prefix getPrefixSections (Logger f) =
  Logger $ \level sections ->
    f level (getPrefixSections level <> sections)

namespace :: Section -> Section -> Section -> (Level -> [Section]) -> Logger -> Logger
namespace left right separator getNamespaceSections =
  prefix (\level -> left : (List.intersperse separator $ getNamespaceSections level) <> [right])

defaultNamespace :: [Section] -> Logger -> Logger
defaultNamespace sections =
  namespace left right' separator $ \level ->
    map (setDefaultLevelFormatting level) (fromLevel False level : sections)
  where
    setGreyForeground = setForeground (Black Bright)
    left = setGreyForeground "["
    right' = setGreyForeground "] "
    separator = setGreyForeground ":"

suffix :: (Level -> [Section]) -> Logger -> Logger
suffix getSuffixSections (Logger f) =
  Logger $ \level sections ->
    f level (sections <> getSuffixSections level)

-- * Level

data Level = Info | Warn | Error | Debug

-- * Section

data Section
  = Section
      { _foreground :: !Color
      , _background :: !Color
      , _style      :: !Style
      , _text       :: !Text
      }

-- | The 'IsString' instance of 'Section' constructs a "plain" 'Section' with no
-- 'Color' or 'Style'.
instance GHC.Exts.IsString Section where
  fromString = plain . GHC.Exts.fromString

-- | The 'Semigroup' instance of 'Section' retains the '_foreground', '_background'
-- and '_style' of the first argument and disregards those values of the second
-- argument. It calls '<>' on the internal '_text' values of each 'Section' to
-- concatenate them.
instance Semigroup Section where
  (<>) a b = a { _text = _text a <> _text b }

-- ** Constructors

styled :: Color -> Color -> Style -> Text -> Section
styled = Section

plain :: Text -> Section
plain = styled Default Default Regular

fromLevel :: Bool -> Level -> Section
fromLevel fixedWidth level =
  plain $ justify $ case level of
    Info  -> "info"
    Warn  -> "warn"
    Error -> "error"
    Debug -> "debug"
  where
    justify = if fixedWidth then Text.justifyLeft 5 ' ' else id

fromShow :: Show a => a -> Section
fromShow = plain . Text.pack . show

fromIntegral :: Integral a => a -> Section
fromIntegral = fromShow . toInteger

fromUUID :: UUID -> Section
fromUUID = plain . UUID.encodeBase64TextStrict

fromDate :: Date -> Section
fromDate =
  plain . Date.Formatter.format (Date.Formatter.iso8601Date Date.Formatter.Day)

fromTimeAndZone :: Time.Zone -> Time -> Section
fromTimeAndZone zone =
  plain . Time.Formatter.format (Time.Formatter.iso8601DateAndTime Time.Formatter.Day Time.Formatter.Millisecond) zone

fromTimeUTC :: Time -> Section
fromTimeUTC = fromTimeAndZone Time.utc

fromYears :: Time.Years -> Section
fromYears = (<> "y") . fromIntegral

fromMonth :: Time.Month -> Section
fromMonth = plain . Month.toText

fromWeeks :: Time.Weeks -> Section
fromWeeks = (<> "w") . fromIntegral

fromWeekday :: Time.Weekday -> Section
fromWeekday = plain . Weekday.toText

fromDays :: Time.Days -> Section
fromDays = (<> "d") . fromIntegral

fromHours :: Time.Hours -> Section
fromHours = (<> "h") . fromIntegral

fromMinutes :: Time.Minutes -> Section
fromMinutes = (<> "m") . fromIntegral

fromSeconds :: Time.Seconds -> Section
fromSeconds = (<> "s") . fromIntegral

fromMilliseconds :: Time.Milliseconds -> Section
fromMilliseconds = (<> "ms") . fromIntegral

fromMicroseconds :: Time.Microseconds -> Section
fromMicroseconds = (<> "µs") . fromIntegral

fromNanoseconds :: Time.Nanoseconds -> Section
fromNanoseconds = (<> "ns") . fromIntegral

fromPicoseconds :: Time.Picoseconds -> Section
fromPicoseconds = (<> "ps") . fromIntegral

-- ** Converters

sectionToText :: Section -> Text
sectionToText (Section {..}) =
  mconcat
    [ colorToSequence Foreground _foreground
    , colorToSequence Background _background
    , styleToSequence _style
    , _text
    , resetSequence
    ]

-- ** Setters

setForeground :: Color -> Section -> Section
setForeground color section = section { _foreground = color }

setBackground :: Color -> Section -> Section
setBackground color section = section { _background = color }

setStyle :: Style -> Section -> Section
setStyle style section = section { _style = style }

setDefaultLevelFormatting :: Level -> Section -> Section
setDefaultLevelFormatting level =
  setStyle Regular
    . setBackground Default
    . setForeground
        (case level of
          Info  -> Cyan Normal
          Warn  -> Yellow Normal
          Error -> Red Normal
          Debug -> Blue Normal)

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
