{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HAYS.Server
    ( Config (..)
    , run
    ) where

import           Data.Bifunctor              (bimap)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as TextEncoding
import           GHC.Exception.Type          (SomeException, fromException)
import           HAYS.Server.Request         (Request)
import qualified HAYS.Server.Request         as Request
import           HAYS.Server.Response        (Response)
import qualified HAYS.Server.Response        as Response
import           HAYS.Server.Route           (Route)
import qualified HAYS.Server.Route           as Route
import qualified Lib.Time                    as Time
import qualified Lib.URL.Component.Path      as Path
import qualified Lib.UUID                    as UUID
import           Logger                      (Logger)
import qualified Logger                      as Logger
import qualified Network.HTTP.Types          as HTTP
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import           System.TimeManager          (TimeoutThread (TimeoutThread))

data Config
  = Config
      { cRoutes      :: [Route]
        -- ^ The list of routes.
      , cNotFound    :: Request -> Route.Action
        -- ^ The response to use when a requested route is not found.
      , cTLSSettings :: Maybe WarpTLS.TLSSettings
        -- ^ The TLS settings to use when running securely.
      , cSettings    :: Maybe Warp.Settings
        -- ^ The Warp settings to use, if any.
      , cMiddleware  :: Maybe Wai.Middleware
        -- ^ The WAI middleware settings to use, if any.
      , cLogger      :: Logger
        -- ^ The logger to use.
      }

run :: Config -> IO ()
run Config {..} =
  runSettings settings
    $ middleware
    $ \waiRequest sendWaiResponse -> do
        -- Track request start time
        start <- Time.now
        -- Transform request
        request <- Request.fromWaiRequest waiRequest
        -- Determine log request ID
        requestLogId <- nextLogId
        -- Log incoming request
        logRequest cLogger requestLogId request
        -- Compute response based on server configuration
        response <- case Route.find request cRoutes of
          Just action -> processAction action
          Nothing     -> processAction $ cNotFound request
        -- Send response to client
        responseSent <- sendWaiResponse $ Response.toWaiResponse response
        -- Track request end time
        end <- Time.now
        let elapsed = end - start
        logResponse cLogger requestLogId response elapsed
        -- Return correct value for WAI
        return responseSent
  where
    runSettings = maybe Warp.runSettings WarpTLS.runTLS cTLSSettings
    settings = Warp.setOnException (logException cLogger) $ fromMaybe Warp.defaultSettings cSettings
    middleware = fromMaybe id cMiddleware
    processAction :: Route.Action -> IO Response
    processAction action = do
      case action of
        Route.Impure io        -> io >>= processAction
        Route.Respond response -> return response

-- Logging

instance Logger.ToRecord SomeException where
  toRecord = Logger.bold (Logger.Level Logger.Error) . Logger.text . Text.pack . show

instance Logger.ToRecord TimeoutThread where
  toRecord = Logger.regular (Logger.Level Logger.Warn) . Logger.text . Text.pack . show

instance Logger.ToRecord Warp.InvalidRequest where
  toRecord = Logger.regular (Logger.Level Logger.Warn) . Logger.text . Text.pack . show

type LogId = UUID.UUID

nextLogId :: IO LogId
nextLogId = UUID.nextRandom

logIdToText :: LogId -> Text
logIdToText = UUID.encodeBase64TextStrict

logRequest :: Logger -> LogId -> Request -> IO ()
logRequest logger logId (method, path, _, _, _) =
  writeRecord (logger Logger.Debug) logId record
    where
      record = Logger.delimitSpace
        [ Logger.leftArrow
        , Logger.boldText (Logger.Level Logger.Info) $ TextEncoding.decodeUtf8 method
        , Logger.text $ Path.toText path
        ]

logResponse :: Logger -> LogId -> Response -> Time.Time -> IO ()
logResponse logger logId (HTTP.Status code _, _, _) elapsed =
  writeRecord (logger Logger.Debug) logId record
    where
      record = Logger.delimitSpace
        [ Logger.rightArrow
        , Logger.bold (colorCode code) $ Logger.toRecord code
        , Logger.regular Logger.Muted $ Logger.toRecord $ Time.toMilliseconds elapsed
        ]

      colorCode code
        | code >= 400 = Logger.Level Logger.Error
        | code >= 300 = Logger.Level Logger.Warn
        | code >= 200 = Logger.Success
        | otherwise = Logger.Muted

writeRecord :: (Logger.Record -> IO ()) -> LogId -> Logger.Record -> IO ()
writeRecord log logId record =
  log
    $ Logger.delimitSpace
        [ Logger.surroundBrackets $ Logger.mutedRegularText $ logIdToText logId
        , record
        ]

logException :: Logger -> Maybe Wai.Request -> SomeException -> IO ()
logException logger maybeRequest exception =
  case fromException exception of
    Just e@TimeoutThread -> logWarn $ record e
    _                    -> case fromException exception of
                              Just e@Warp.ConnectionClosedByPeer -> logWarn $ record e
                              _ -> logError $ record exception
    where
      record :: Logger.ToRecord r => r -> Logger.Record
      record r = Logger.delimitSpace $ waiRequestRecord <> [Logger.toRecord r]
      waiRequestRecord =
        case maybeRequest of
          Nothing -> []
          Just request ->
            [ Logger.delimitSpace
                $ map (Logger.text . Text.pack)
                    [ show $ Wai.requestMethod request
                    , show $ Wai.httpVersion request
                    , show $ Wai.pathInfo request
                    ]
            ]
      logWarn = logger Logger.Warn
      logError = logger Logger.Error
