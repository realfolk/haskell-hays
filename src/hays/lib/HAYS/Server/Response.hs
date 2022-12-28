{-# LANGUAGE RecordWildCards #-}

module HAYS.Server.Response
    ( Body
    , Header
    , Response
    , Status
    , getBody
    , getHeaders
    , getStatus
    , new
    , toWaiResponse
    ) where

import           Blaze.ByteString.Builder (fromLazyByteString)
import           HAYS.Server.Internal.HTTP       (Body, Header, Status)
import qualified Network.Wai              as Wai

-- * Response

data Response
  = Response
      { _status          :: Status
      , _responseHeaders :: [Header]
      , _responseBody    :: Body
      }

-- ** Constructors

new :: Status -> [Header] -> Body -> Response
new = Response

-- ** Converters

toWaiResponse :: Response -> Wai.Response
toWaiResponse Response{..} =
  Wai.responseBuilder _status _responseHeaders (fromLazyByteString _responseBody)

-- ** Getters

getStatus :: Response -> Status
getStatus = _status

getHeaders :: Response -> [Header]
getHeaders = _responseHeaders

getBody :: Response -> Body
getBody = _responseBody
