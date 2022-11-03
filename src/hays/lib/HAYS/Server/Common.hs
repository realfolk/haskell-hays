module HAYS.Server.Common
    ( Body
    , Headers
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types   as H

type Headers = [H.Header]

type Body = LBS.ByteString
