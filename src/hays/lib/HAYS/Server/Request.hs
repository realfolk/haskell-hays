module HAYS.Server.Request
    ( Body
    , Header
    , Method
    , Path
    , Query
    , Request
    , consumeHeaderBy
    , consumeNextPathFragment
    , consumeQueryItemBy
    , fromWaiRequest
    , getBody
    , getHeaders
    , getMethod
    , getPath
    , getQuery
    ) where

import           Control.Monad.IO.Class       (MonadIO)
import qualified Control.Monad.IO.Class       as Monad.IO
import qualified Data.List                    as List
import           HAYS.Server.Internal.HTTP    (Body, Header, HeaderName, Method)
import           Lib.URL.Component.Path       (Path)
import qualified Lib.URL.Component.Path       as Path
import           Lib.URL.Component.Query      (Query)
import qualified Lib.URL.Component.Query      as Query
import qualified Lib.URL.Component.Query.Item as Query.Item
import qualified Network.Wai                  as Wai

-- * Request

data Request
  = Request
      { _method  :: Method
      , _path    :: Path
      , _query   :: Query
      , _headers :: [Header]
      , _body    :: Body
      }

-- ** Constructors

fromWaiRequest :: MonadIO m => Wai.Request -> m Request
fromWaiRequest req =
  Request method path query headers <$> body
  where
    method = Wai.requestMethod req
    path = Path.fromWaiRequest req
    query = Query.fromWaiRequest req
    headers = Wai.requestHeaders req
    body = Monad.IO.liftIO $ Wai.lazyRequestBody req

-- ** Getters

getMethod :: Request -> Method
getMethod = _method

getPath :: Request -> Path
getPath = _path

getQuery :: Request -> Query
getQuery = _query

getHeaders :: Request -> [Header]
getHeaders = _headers

getBody :: Request -> Body
getBody = _body

-- ** Modifiers

consumeNextPathFragment :: Request -> Maybe (Path.Section, Request)
consumeNextPathFragment request = do
  (section, newPath) <- Path.uncons $ getPath request
  return (section, request { _path = newPath })

consumeHeaderBy :: (Header -> Bool) -> Request -> Maybe (Header, Request)
consumeHeaderBy f request =
  case List.partition f (getHeaders request) of
    ([], _) -> Nothing
    (match:otherMatches, nonMatches) ->
      Just (match, request { _headers = otherMatches ++ nonMatches })

consumeQueryItemBy :: (Query.Item.Item -> Bool) -> Request -> Maybe (Query.Item.Item, Request)
consumeQueryItemBy f request = do
  let (maybeQueryItem, nonMatches) = Query.findItem f (getQuery request)
  queryItem <- maybeQueryItem
  return (queryItem, request { _query = nonMatches })
