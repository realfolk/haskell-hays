{-# LANGUAGE OverloadedStrings #-}

module Test.HAYS.LoggerSpec
    ( spec
    ) where

import qualified Control.Concurrent as Concurrent
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           HAYS.Logger        (Logger)
import qualified HAYS.Logger        as Logger
import           Test.Hspec

testLogger :: IO (Concurrent.Chan (Logger.Level, Text), Logger)
testLogger = do
  chan <- Concurrent.newChan
  return
    ( chan
    , Logger.fromIO $ curry $ Concurrent.writeChan chan
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
      describe "fromIO" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "fromHandle" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
      describe "defaultTerminal" $ do
        context "when" $ do
          it "it" $ do
            True `shouldBe` True
    describe "Logging Functions" $ do
      let singleTest log record expectedLevel expectedMessage = do
            (chan, logger) <- testLogger
            log logger record
            Concurrent.readChan chan `shouldReturn` (expectedLevel, expectedMessage)
          makeTests log level = do
            context "when the logged record is formatted" $ do
              it "logs the message with the correct formatting" $
                singleTest log formattedRecord0 level formattedMessage0
            context "when the logged record is not formatted" $ do
              it "logs the message as plain text" $ do
                singleTest log plainRecord0 level plainMessage0
      describe "info" $ makeTests Logger.info Logger.Info
      describe "warn" $ makeTests Logger.warn Logger.Warn
      describe "error'" $ makeTests Logger.error' Logger.Error
      describe "debug" $ makeTests Logger.debug Logger.Debug
    describe "Modifiers" $ do
      describe "prefix" $ do
        let makeTest makePrefix record makeExpectedMessage = do
              (chan, logger) <- testLogger
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
        context "when the prefix is formatted" $ do
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
        context "when the prefix is not formatted" $ do
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
      describe "Show" $ do
        describe "show" $ do
          it "it" $ do
            True `shouldBe` True
  describe "Record" $ do
    describe "Instances" $ do
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
      describe "toText" $ do
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
