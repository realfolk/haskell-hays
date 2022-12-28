{-# LANGUAGE OverloadedStrings #-}

module HAYS.Server.Internal.HTTP
    ( Body
    , Header
    , HeaderName
    , HeaderValue
    , Method
    , Status
    , jsonMimeType
    ) where

import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.HTTP.Types   as HTTP

type Method = HTTP.Method

type HeaderName = HTTP.HeaderName

type HeaderValue = ByteString.ByteString

type Header = HTTP.Header

type Body = ByteString.Lazy.ByteString

type Status = HTTP.Status

jsonMimeType :: HeaderValue
jsonMimeType = "application/json"
