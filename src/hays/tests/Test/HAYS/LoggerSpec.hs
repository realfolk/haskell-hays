{-# LANGUAGE OverloadedStrings #-}

module Test.HAYS.LoggerSpec
    ( spec
    ) where

import qualified Control.Concurrent as Concurrent
import qualified Data.Bifunctor     as Bifunctor
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           HAYS.Logger        (Logger)
import qualified HAYS.Logger        as Logger
import           Test.Hspec

testLogger :: (Logger.Level -> Logger.Record -> a) -> IO (Concurrent.Chan a, Logger)
testLogger f = do
  chan <- Concurrent.newChan
  return
    ( chan
    , Logger.fromIO $ \level record -> Concurrent.writeChan chan $ f level record
    )

plainRecord0 = Logger.plain "0"

plainMessage0 = "0" :: Text

plainRecord1 = Logger.plain "1"

plainMessage1 = "1" :: Text

makeFormattedRecord =
  Logger.formatted (Logger.Red Logger.Normal) (Logger.White Logger.Bright) Logger.Bold

makeFormattedMessage (text :: Text) =
  "\ESC[31m\ESC[107m\ESC[1m" <> text <> "\ESC[0m"

formattedRecord0 = makeFormattedRecord "0"

formattedMessage0 = makeFormattedMessage "0"

formattedRecord1 = makeFormattedRecord "1"

formattedMessage1 = makeFormattedMessage "1"

spec :: Spec
spec = do
  describe "Logger" $ do
    describe "Constructors" $ do
      describe "fromTextHandle" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromByteStringHandle" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "defaultTerminal" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
    describe "Logging Functions" $ do
      let makeTest log expectedLevel record =
            it "logs the record unchanged with the correct level" $ do
              (chan, logger) <- testLogger (,)
              log logger record
              Concurrent.readChan chan `shouldReturn` (expectedLevel, record)
      describe "info" $ makeTest Logger.info Logger.Info formattedRecord0
      describe "warn" $ makeTest Logger.warn Logger.Warn formattedRecord0
      describe "error'" $ makeTest Logger.error' Logger.Error formattedRecord0
      describe "debug" $ makeTest Logger.debug Logger.Debug formattedRecord0
    describe "Modifiers" $ do
      describe "prefix" $ do
        let makeTest makePrefix record makeExpectedMessage = do
              (chan, logger) <- testLogger $ curry $ Bifunctor.second Logger.toANSIFormattedText
              let prefixedLogger = Logger.prefix makePrefix logger
              let makeExpectedResult level = (level, makeExpectedMessage level)
              Logger.info prefixedLogger record
              Concurrent.readChan chan `shouldReturn` makeExpectedResult Logger.Info
              Logger.warn prefixedLogger record
              Concurrent.readChan chan `shouldReturn` makeExpectedResult Logger.Warn
              Logger.error' prefixedLogger record
              Concurrent.readChan chan `shouldReturn` makeExpectedResult Logger.Error
              Logger.debug prefixedLogger record
              Concurrent.readChan chan `shouldReturn` makeExpectedResult Logger.Debug
        context "when the logger converts the record to ANSI-formatted text" $ do
          context "and the prefix is formatted" $ do
            context "and the logged record is formatted" $ do
              it "logs the prefix and message with their formatting preserved" $
                let levelToText = Text.pack . show
                    makePrefix = makeFormattedRecord . levelToText
                    makeExpectedMessage level =
                      makeFormattedMessage (levelToText level) <> formattedMessage0
                 in makeTest makePrefix formattedRecord0 makeExpectedMessage
            context "and the logged record is not formatted" $ do
              it "logs the prefix with its formatting preserved and the message as plain text" $
                let levelToText = Text.pack . show
                    makePrefix = makeFormattedRecord . levelToText
                    makeExpectedMessage level =
                      makeFormattedMessage (levelToText level) <> "0"
                 in makeTest makePrefix plainRecord0 makeExpectedMessage
          context "and the prefix is not formatted" $ do
            context "and the logged record is formatted" $ do
              it "logs the prefix as plain text and the message with its formatting preserved" $
                let levelToText = Text.pack . show
                    makePrefix = Logger.plain . levelToText
                    makeExpectedMessage level =
                      levelToText level <> formattedMessage0
                 in makeTest makePrefix formattedRecord0 makeExpectedMessage
            context "and the logged record is not formatted" $ do
              it "logs the prefix and message as plain text" $
                let levelToText = Text.pack . show
                    makePrefix = Logger.plain . levelToText
                    makeExpectedMessage level =
                      levelToText level <> plainMessage0
                 in makeTest makePrefix plainRecord0 makeExpectedMessage
      describe "namespace" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "defaultNamespace" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "suffix" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
  describe "Level" $ do
    describe "Instances" $ do
      describe "Eq" $ do
        describe "(==)" $ do
          it "it" $ do
            True `shouldBe` True
      describe "Show" $ do
        describe "show" $ do
          it "it" $ do
            True `shouldBe` True
  describe "Record" $ do
    describe "Instances" $ do
      describe "Eq" $ do
        describe "(==)" $ do
          context "when" $ do
            it "it" $ do
              True `shouldBe` True
      describe "Show" $ do
        describe "show" $ do
          context "when" $ do
            it "it" $ do
              True `shouldBe` True
      describe "Semigroup" $ do
        describe "(<>)" $ do
          context "when" $ do
            it "it" $ do
              True `shouldBe` True
      describe "GHC.Exts.IsString" $ do
        describe "fromString" $ do
          context "when" $ do
            it "it" $ do
              True `shouldBe` True
      describe "GHC.Exts.IsList" $ do
        describe "fromList" $ do
          context "when" $ do
            it "it" $ do
              True `shouldBe` True
    describe "Constructors" $ do
      describe "plain" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "formatted" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromLevel" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromShow" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromIntegral" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromUUID" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromDate" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromTimeAndZone" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromTimeUTC" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromYears" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromMonth" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromWeeks" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromWeekday" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromDays" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromHours" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromMinutes" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromSeconds" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromMilliseconds" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromMicroseconds" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromNanoseconds" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromPicoseconds" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
    describe "Converters" $ do
      describe "toPlainText" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "toANSIFormattedText" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
    describe "Modifiers" $ do
      describe "setForeground" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "setBackground" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "setStyle" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "setDefaultLevelFormatting" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "removeFormatting" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
  describe "Color" $ do
    describe "Instances" $ do
      describe "Eq" $ do
        describe "(==)" $ do
          it "it" $ do
            True `shouldBe` True
      describe "Show" $ do
        describe "show" $ do
          it "it" $ do
            True `shouldBe` True
    describe "Converters" $ do
      describe "colorToANSISequence" $ do
        it "it" $ do
          True `shouldBe` True
  describe "ColorIntensity" $ do
    describe "Instances" $ do
      describe "Eq" $ do
        describe "(==)" $ do
          it "it" $ do
            True `shouldBe` True
      describe "Show" $ do
        describe "show" $ do
          it "it" $ do
            True `shouldBe` True
  describe "ColorTarget" $ do
    describe "Instances" $ do
      describe "Eq" $ do
        describe "(==)" $ do
          it "it" $ do
            True `shouldBe` True
      describe "Show" $ do
        describe "show" $ do
          it "it" $ do
            True `shouldBe` True
  describe "Style" $ do
    describe "Instances" $ do
      describe "Eq" $ do
        describe "(==)" $ do
          it "it" $ do
            True `shouldBe` True
      describe "Show" $ do
        describe "show" $ do
          it "it" $ do
            True `shouldBe` True
    describe "Converters" $ do
      describe "styleToANSISequence" $ do
        it "it" $ do
          True `shouldBe` True
