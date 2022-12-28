{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HAYS.Server
    ( Error (..)
    , Port
    , Server
    , defaultLogger
    , defaultServer
    , listen
    , setLogger
    , setOnErrorIO
    , setOnErrorResponse
    , setOnMsg
    , setPort
    , setRouter
    , setWaiMiddleware
    , setWarpSettings
    , setWarpTLSSettings
    ) where

import           Data.Function               ((&))
import qualified Data.Text.Encoding          as Text.Encoding
import           Data.Word                   (Word16)
import           GHC.Exception.Type          (SomeException)
import           HAYS.Logger                 (Logger)
import qualified HAYS.Logger                 as Logger
import           HAYS.Server.Request         (Request)
import qualified HAYS.Server.Request         as Request
import           HAYS.Server.Response        (Response)
import qualified HAYS.Server.Response        as Response
import           HAYS.Server.Router          (Router)
import qualified HAYS.Server.Router          as Router
import           Lib.Time                    (Time)
import qualified Lib.Time                    as Time
import qualified Lib.URL.Component.Path      as Path
import qualified Lib.URL.Component.Query     as Query
import           Lib.UUID                    (UUID)
import qualified Lib.UUID                    as UUID
import qualified Network.HTTP.Types          as HTTP
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

-- * Server

data Server msg
  = Server
      { _port            :: Port
      , _warpSettings    :: Warp.Settings
      , _warpTLSSettings :: Maybe WarpTLS.TLSSettings
      , _waiMiddleware   :: Wai.Middleware
      , _logger          :: UUID -> Logger
      , _router          :: Router msg
      , _onMsg           :: msg -> IO Response
      , _onErrorResponse :: Error -> Response
      , _onErrorIO       :: Error -> IO ()
      }

-- ** Constructors

defaultServer :: Server msg
defaultServer =
  Server
    3000
    Warp.defaultSettings
    Nothing
    id
    defaultLogger
    Router.next
    (\_ -> (pure defaultResponse))
    (\_ -> defaultResponse)
    (\_ -> return ())
  where
    defaultResponse =
      Response.new
        HTTP.status500
        []
        "Default Server Response"

-- ** Execution

listen :: Server msg -> IO ()
listen (Server {..}) =
  startServer $ \waiRequest sendWaiResponse -> do
    -- Track request start time
    startTime <- Time.now
    -- Transform request
    request <- Request.fromWaiRequest waiRequest
    -- Determine log request ID
    requestID <- nextLogID
    let logger = _logger requestID
    -- Log incoming request
    logRequest logger request
    -- Compute response based on server configuration
    response <- case Router.route _router request of
      Just msg -> _onMsg msg
      Nothing  -> do
        let error' = UnhandledRequest request
        _onErrorIO error'
        return $ _onErrorResponse error'
    -- Send response to client
    responseSent <- sendWaiResponse $ Response.toWaiResponse response
    -- Track request end time
    endTime <- Time.now
    let elapsedTime = endTime - startTime
    logResponse logger response elapsedTime
    -- Return correct value for WAI
    return responseSent
  where
    warpSettings =
      _warpSettings
        & Warp.setOnException (\_ e -> _onErrorIO (WarpException e))
        & Warp.setOnExceptionResponse (Response.toWaiResponse . _onErrorResponse . WarpException)
    startServer =
      (maybe Warp.runSettings WarpTLS.runTLS _warpTLSSettings) warpSettings . _waiMiddleware

-- ** Setters

setPort :: Port -> Server msg -> Server msg
setPort port server = server { _port = port }

setWarpSettings :: Warp.Settings -> Server msg -> Server msg
setWarpSettings warpSettings server = server { _warpSettings = warpSettings }

setWarpTLSSettings :: Maybe WarpTLS.TLSSettings -> Server msg -> Server msg
setWarpTLSSettings warpTLSSettings server = server { _warpTLSSettings = warpTLSSettings }

setWaiMiddleware :: Wai.Middleware -> Server msg -> Server msg
setWaiMiddleware waiMiddleware server = server { _waiMiddleware = waiMiddleware }

setLogger :: (UUID -> Logger) -> Server msg -> Server msg
setLogger logger server = server { _logger = logger }

setRouter :: Router msg -> Server msg -> Server msg
setRouter router server = server { _router = router }

setOnMsg :: (msg -> IO Response) -> Server msg -> Server msg
setOnMsg onMsg server = server { _onMsg = onMsg }

setOnErrorResponse :: (Error -> Response) -> Server msg -> Server msg
setOnErrorResponse onErrorResponse server = server { _onErrorResponse = onErrorResponse }

setOnErrorIO :: (Error -> IO ()) -> Server msg -> Server msg
setOnErrorIO onErrorIO server = server { _onErrorIO = onErrorIO }

-- * Error

data Error
  = UnhandledRequest Request
  | WarpException SomeException

-- * Port

type Port = Word16

-- * Logging

defaultLogger :: UUID -> Logger
defaultLogger requestID =
  Logger.defaultNamespace [Logger.fromUUID requestID] Logger.terminal

-- ** Internal

nextLogID :: IO UUID
nextLogID = UUID.nextRandom

logRequest :: Logger -> Request -> IO ()
logRequest logger request =
  Logger.debug logger sections
    where
      query = Request.getQuery request
      sections =
        [ "<- "
        , Logger.setStyle Logger.Bold
            $ Logger.plain
            $ Text.Encoding.decodeUtf8
            $ Request.getMethod request
        , " "
        , Logger.plain $ Path.toText $ Request.getPath request
        , setGreyForeground
            $ if Query.isEmpty query
               then ""
               else "?" <> Logger.plain (Query.toText query)
        ]

logResponse :: Logger -> Response -> Time -> IO ()
logResponse logger response elapsedTime =
  Logger.debug logger sections
    where
      sections =
        [ "-> "
        , Logger.fromShow statusCode
            & Logger.setForeground (statusColor statusCode)
            & Logger.setStyle Logger.Bold
        , " "
        , setGreyForeground
            $ Logger.fromMilliseconds
            $ Time.toMilliseconds elapsedTime
        ]
      statusCode = HTTP.statusCode $ Response.getStatus response
      statusColor statusCode
        | statusCode >= 400 = Logger.Red Logger.Normal
        | statusCode >= 300 = Logger.Yellow Logger.Normal
        | statusCode >= 200 = Logger.Green Logger.Normal
        | otherwise = Logger.Default

setGreyForeground :: Logger.Section -> Logger.Section
setGreyForeground = Logger.setForeground (Logger.Black Logger.Bright)
