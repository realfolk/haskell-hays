{-# LANGUAGE OverloadedStrings #-}

module HAYS.Server.Router
    ( Router
    , getBody
    , getHeaderValue
    , getMethod
    , getNextPathSection
    , getQueryItemValue
    , getRequest
    , headerValueIs
    , isAcceptingJSON
    , isPathEnd
    , isProvidingJSON
    , isQueryEnd
    , json
    , methodIs
    , next
    , nextPathSectionIs
    , parseJSONBody
    , queryItemIs
    , route
    , (<|>)
    ) where

import           Control.Applicative          (Alternative, asum, (<|>))
import           Control.Monad                (unless)
import           Control.Monad.Reader         (ReaderT)
import qualified Control.Monad.Reader         as Reader
import           Control.Monad.State.Strict   (StateT)
import qualified Control.Monad.State.Strict   as State
import qualified Data.Aeson                   as Aeson
import           Data.Bifunctor               (Bifunctor (first))
import qualified Data.ByteString              as ByteString
import           Data.Text                    (Text)
import           HAYS.Server.Internal.HTTP    (Body, Header, HeaderName,
                                               HeaderValue, Method,
                                               jsonMimeType)
import           HAYS.Server.Request          (Request)
import qualified HAYS.Server.Request          as Request
import qualified Lib.URL.Component.Query.Item as Query.Item
import qualified Network.HTTP.Types           as HTTP

-- * Router

newtype Router a
  = Router (ReaderT Request (StateT Request Maybe) a)
  deriving (Alternative, Applicative, Functor, Monad)

-- ** Execution

route :: Router a -> Request -> Maybe a
route (Router m) request = fmap fst $ State.runStateT (Reader.runReaderT m request) request

-- ** Constructors

next :: Router a
next = liftMaybe Nothing

-- ** Combinators

combine :: (Foldable t) => t (Router a) -> Router a
combine = asum

-- ** Parsers

-- *** Request

getRequest :: Router Request
getRequest = Router $ Reader.ask

-- *** Method

getMethod :: Router Method
getMethod = getRemainingRequestComponent Request.getMethod

methodIs :: (Method -> Bool) -> Router ()
methodIs f = do
  method <- getMethod
  unless (f method) next

-- *** Path

getNextPathSection :: Router Text
getNextPathSection = withRemainingRequest
  (maybe next return . Request.consumeNextPathFragment)

nextPathSectionIs :: (Text -> Bool) -> Router ()
nextPathSectionIs f = do
  section <- getNextPathSection
  unless (f section) next

isPathEnd :: Router ()
isPathEnd = withRemainingRequest $ \request ->
  maybe (return ((), request)) (const next) (Request.consumeNextPathFragment request)

-- *** Query

getQueryItemValue :: Text -> Router Text
getQueryItemValue key = withRemainingRequest
  (maybe next (return . first snd) . Request.consumeQueryItemBy ((== key) . fst))

queryItemIs :: (Query.Item.Item -> Bool) -> Router ()
queryItemIs f = withRemainingRequest
  (maybe next (return . first (const ())) . Request.consumeQueryItemBy f)

isQueryEnd :: Router ()
isQueryEnd = withRemainingRequest $ \request ->
  maybe (return ((), request)) (const next) (Request.consumeQueryItemBy (const True) request)

-- *** Headers

getHeaderValue :: HeaderName -> Router HeaderValue
getHeaderValue name = withRemainingRequest
  (maybe next (return . first snd) . Request.consumeHeaderBy ((== name) . fst))

headerValueIs :: HeaderName -> (HeaderValue -> Bool) -> Router ()
headerValueIs name f = do
  value <- getHeaderValue name
  unless (f value) next

isProvidingJSON :: Router ()
isProvidingJSON = headerValueIs HTTP.hContentType (ByteString.isInfixOf jsonMimeType)

isAcceptingJSON :: Router ()
isAcceptingJSON = headerValueIs HTTP.hAccept (ByteString.isInfixOf jsonMimeType)

-- *** Body

getBody :: Router Body
getBody = Request.getBody <$> getRequest

parseJSONBody :: Aeson.FromJSON a => Router a
parseJSONBody = do
  body <- getBody
  maybe next return $ Aeson.decode body

-- *** Composite

json :: Aeson.FromJSON a => Router a
json = isProvidingJSON >> parseJSONBody

-- ** Internal

liftMaybe :: Maybe a -> Router a
liftMaybe = Router . maybe (Reader.lift (Reader.lift Nothing)) return

getRemainingRequest :: Router Request
getRemainingRequest = Router $ State.get

getRemainingRequestComponent :: (Request -> a) -> Router a
getRemainingRequestComponent = Router . State.gets

putRemainingRequest :: Request -> Router ()
putRemainingRequest = Router . State.put

withRemainingRequest :: (Request -> Router (a, Request)) -> Router a
withRemainingRequest f = do
  request <- getRemainingRequest
  (result, newRequest) <- f request
  putRemainingRequest newRequest
  return result
