{-# LANGUAGE OverloadedStrings #-}

module HAYS.Server.Cache
    ( expire
    , none
    , validate
    ) where

import qualified Data.ByteString.Char8     as BS8
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.List                 as L
import           HAYS.Server.Response      (Headers, Response)
import qualified Lib.Base64                as B64
import qualified Lib.Crypto.Hash           as Hash
import qualified Lib.Crypto.Hash.SHA1      as SHA1
import qualified Lib.Time                  as Time
import qualified Network.HTTP.Types        as H
import qualified Network.HTTP.Types.Header as H

none :: Response -> Response
none (status, headers, body) = (status, noStoreHeader : headers, body)
  where
    noStoreHeader = (H.hCacheControl, "no-store")

expire :: Time.Seconds -> Headers -> Response -> Response
expire = cacheResponse . Expire

validate :: Headers -> Response -> Response
validate = cacheResponse Validate

data CachingStrategy
  = Expire Time.Seconds
  | Validate

cacheResponse :: CachingStrategy -> Headers -> Response -> Response
cacheResponse strategy requestHeaders (status, headers, body) =
  case strategy of
    Expire limit -> hashAndCompare (maxAgeHeader limit)
    Validate     -> hashAndCompare noCacheHeader
  where
    hashAndCompare cacheControlHeader = do
      let contentHash = B64.encodeStrict $ Hash.encodeStrict $ SHA1.hash body
      if cacheHit contentHash requestHeaders
         then respond304
         else (status, cacheControlHeader : eTagHeader contentHash : headers, body)
    respond304 = (H.status304, [], LBS.empty)
    maxAgeHeader limit = (H.hCacheControl, "private,immutable,max-age=" <> BS8.pack (show (toInteger limit)))
    noCacheHeader = (H.hCacheControl, "no-cache")
    eTagHeader contentHash = (H.hETag, contentHash)
    cacheHit contentHash = L.elem (H.hIfNoneMatch, contentHash)
