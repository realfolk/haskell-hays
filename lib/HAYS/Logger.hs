{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module HAYS.Logger
    ( Color (..)
    , ColorIntensity (..)
    , GHC.Exts.fromList
    , Level (..)
    , Logger
    , Record
    , Style (..)
    , debug
    , defaultNamespace
    , defaultTerminal
    , error'
    , formatted
    , fromByteStringHandle
    , fromDate
    , fromDays
    , fromHours
    , fromIO
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
    , fromTextHandle
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
    , removeFormatting
    , setBackground
    , setDefaultLevelFormatting
    , setForeground
    , setStyle
    , silent
    , suffix
    , toANSIFormattedText
    , toPlainText
    , warn
    ) where

import           Control.Monad.IO.Class    (MonadIO)
import qualified Control.Monad.IO.Class    as MonadIO
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import qualified GHC.Exts
import           Pouch.Time                (Time)
import qualified Pouch.Time                as Time
import           Pouch.Time.Date           (Date)
import qualified Pouch.Time.Date           as Date
import qualified Pouch.Time.Date.Formatter as Date.Formatter
import qualified Pouch.Time.Formatter      as Time.Formatter
import qualified Pouch.Time.Month          as Month
import qualified Pouch.Time.Weekday        as Weekday
import           Pouch.UUID                (UUID)
import qualified Pouch.UUID                as UUID
import           Prelude                   hiding (fromIntegral)
import qualified System.IO                 as IO

-- * Logger

newtype Logger
  = Logger (Level -> Record -> IO ())

-- ** Constructors

fromIO :: (Level -> Record -> IO ()) -> Logger
fromIO = Logger

-- Adds a new line after each logged record.
fromTextHandle :: (Record -> Text) -> (Level -> IO.Handle) -> Logger
fromTextHandle toText getHandle =
  fromIO $ \level record ->
    Text.IO.hPutStrLn (getHandle level) (toText record)

-- Does not delimit logged records, simply appends to the handle.
fromByteStringHandle :: (Record -> ByteString) -> (Level -> IO.Handle) -> Logger
fromByteStringHandle toByteString getHandle =
  fromIO $ \level record ->
    ByteString.hPut (getHandle level) (toByteString record)

silent :: Logger
silent = fromIO $ \_ _ -> return ()

defaultTerminal :: (Record -> Text) -> Logger
defaultTerminal toText =
  fromTextHandle toText $ \level ->
    case level of
      Error -> IO.stderr
      _     -> IO.stdout

-- ** Logging Functions

info :: MonadIO m => Logger -> Record -> m ()
info (Logger f) = MonadIO.liftIO . f Info

warn :: MonadIO m => Logger -> Record -> m ()
warn (Logger f) = MonadIO.liftIO . f Warn

error' :: MonadIO m => Logger -> Record -> m ()
error' (Logger f) = MonadIO.liftIO . f Error

debug :: MonadIO m => Logger -> Record -> m ()
debug (Logger f) = MonadIO.liftIO . f Debug

-- ** Modifiers

prefix :: (Level -> Record) -> Logger -> Logger
prefix getPrefixRecord (Logger f) =
  Logger $ \level record ->
    f level (getPrefixRecord level <> record)

namespace :: Record -> Record -> Record -> (Level -> [Record]) -> Logger -> Logger
namespace left right separator getNamespaceRecords =
  prefix (\level -> left <> GHC.Exts.fromList (List.intersperse separator (getNamespaceRecords level)) <> right)

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
  deriving (Eq)

instance Show Record where
  show = Text.unpack . toPlainText

instance Semigroup Record where
  (<>) a b =
    case (a, b) of
      (Plain a', Plain b')       -> Plain $ a' <> b'
      (Multiple as, Multiple bs) -> Multiple $ as <> bs
      (Multiple as, _)           -> Multiple $ as <> [b]
      (_, Multiple bs)           -> Multiple $ NonEmpty.cons a bs
      _                          -> Multiple [a, b]

-- | The 'IsString' instance of 'Record' constructs a 'plain' 'Record'
-- with no formatting.
instance GHC.Exts.IsString Record where
  fromString = plain . GHC.Exts.fromString

instance GHC.Exts.IsList Record where
  type Item Record = Record
  fromList records =
    case records of
      []     -> plain ""
      (s:ss) -> Multiple $ s :| ss
  toList record =
    case record of
      Multiple records -> NonEmpty.toList records
      _                -> [record]

-- ** Constructors

plain :: Text -> Record
plain = Plain

formatted :: Color -> Color -> Style -> Text -> Record
formatted = Formatted

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

toPlainText :: Record -> Text
toPlainText record =
  case record of
    Plain text -> text
    Formatted {..} -> _text
    Multiple records ->
      foldr ((<>) . toPlainText) "" records

toANSIFormattedText :: Record -> Text
toANSIFormattedText record =
  case record of
    Plain text -> text
    Formatted {..} ->
      mconcat
        [ colorToANSISequence Foreground _foreground
        , colorToANSISequence Background _background
        , styleToANSISequence _style
        , _text
        , resetSequence
        ]
    Multiple records ->
      foldr ((<>) . toANSIFormattedText) "" records


-- ** Modifiers

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
  deriving (Eq, Show)

-- ** Converters

colorToANSISequence :: ColorTarget -> Color -> Text
colorToANSISequence target color =
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

data ColorIntensity = Normal | Bright deriving (Eq, Show)

-- ** ColorTarget

data ColorTarget = Foreground | Background deriving (Eq, Show)

-- * Style

data Style = Regular | Bold | Dim | Italic | Underline | Blink deriving
    ( Eq
    , Show
    )

-- ** Converters

styleToANSISequence :: Style -> Text
styleToANSISequence style =
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
