{-# LANGUAGE OverloadedStrings #-}

module HAYS.Server.Response.CachingStrategy
    ( CachingStrategy
    , alwaysValidate
    , apply
    , neverCache
    , none
    , validateAfter
    ) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as ByteString.Char8
import qualified Data.ByteString.Lazy      as ByteString.Lazy
import qualified Data.List                 as List
import           HAYS.Server.Internal.HTTP        (Body, Header, Status)
import           HAYS.Server.Response             (Response)
import qualified HAYS.Server.Response             as Response
import qualified Lib.Base64                as Base64
import qualified Lib.Crypto.Hash           as Hash
import qualified Lib.Crypto.Hash.SHA1      as SHA1
import qualified Lib.Time                  as Time
import qualified Network.HTTP.Types        as HTTP
import qualified Network.HTTP.Types.Header as HTTP

-- * CachingStrategy

newtype CachingStrategy
  = CachingStrategy (Response -> Response)

-- ** Constructors

none :: CachingStrategy
none = CachingStrategy id

neverCache :: CachingStrategy
neverCache = CachingStrategy $ \response ->
  Response.new
    (Response.getStatus response)
    (noStoreHeader : Response.getHeaders response)
    (Response.getBody response)

validateAfter :: Time.Seconds -> [Header] -> CachingStrategy
validateAfter seconds
  | seconds <= 0 = alwaysValidate
  | otherwise = CachingStrategy . validate (After seconds)

alwaysValidate :: [Header] -> CachingStrategy
alwaysValidate = CachingStrategy . validate Always

-- ** Application

apply :: CachingStrategy -> Response -> Response
apply (CachingStrategy f) = f

-- ** Helpers

data ValidationStrategy
  = After Time.Seconds
  | Always

validate :: ValidationStrategy -> [Header] -> Response -> Response
validate strategy requestHeaders response =
  if hasHitCache cacheKey requestHeaders
     then cacheHitResponse
     else cacheMissResponse strategy cacheKey response
  where
    cacheKey = newCacheKey $ Response.getBody response

-- *** CacheKey

newtype CacheKey
  = CacheKey ByteString

newCacheKey :: Body -> CacheKey
newCacheKey = CacheKey . Base64.encodeStrict . Hash.encodeStrict . SHA1.hash

hasHitCache :: CacheKey -> [Header] -> Bool
hasHitCache cacheKey requestHeaders = List.elem (ifNoneMatchHeader cacheKey) requestHeaders

-- *** Headers

noStoreHeader :: Header
noStoreHeader = (HTTP.hCacheControl, "no-store")

maxAgeHeader :: Time.Seconds -> Header
maxAgeHeader seconds = (HTTP.hCacheControl, "private,immutable,max-age=" <> ByteString.Char8.pack (show (toInteger seconds)))

noCacheHeader :: Header
noCacheHeader = (HTTP.hCacheControl, "no-cache")

ifNoneMatchHeader :: CacheKey -> Header
ifNoneMatchHeader (CacheKey cacheKey) = (HTTP.hIfNoneMatch, cacheKey)

eTagHeader :: CacheKey -> Header
eTagHeader (CacheKey cacheKey) = (HTTP.hETag, cacheKey)

-- *** Responses

cacheHitResponse :: Response
cacheHitResponse = Response.new HTTP.status304 [] ByteString.Lazy.empty

cacheMissResponse :: ValidationStrategy -> CacheKey -> Response -> Response
cacheMissResponse strategy cacheKey response =
  Response.new
    (Response.getStatus response)
    (cacheControlHeader : eTagHeader cacheKey : Response.getHeaders response)
    (Response.getBody response)
    where
      cacheControlHeader =
        case strategy of
          After seconds -> maxAgeHeader seconds
          Always        -> noCacheHeader
