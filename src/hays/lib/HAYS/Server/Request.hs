{-# LANGUAGE TupleSections #-}

module HAYS.Server.Request
    ( Body
    , Headers
    , Method
    , Path
    , Query
    , QueryItem
    , Request
    , fromWaiRequest
    ) where

import           HAYS.Server.Common           (Body, Headers)
import           Lib.URL.Component.Path       (Path)
import qualified Lib.URL.Component.Path       as Path
import           Lib.URL.Component.Query      (Query)
import qualified Lib.URL.Component.Query      as Query
import qualified Lib.URL.Component.Query.Item as QueryItem
import qualified Network.HTTP.Types           as H
import qualified Network.Wai                  as Wai

type Request = (Method, Path, Query, Headers, Body)

type QueryItem = QueryItem.Item

type Method = H.Method

fromWaiRequest :: Wai.Request -> IO Request
fromWaiRequest req =
  (method, path, query, headers, ) <$> body
  where
    method = Wai.requestMethod req
    path = Path.fromWaiRequest req
    query = Query.fromWaiRequest req
    headers = Wai.requestHeaders req
    body = Wai.lazyRequestBody req
