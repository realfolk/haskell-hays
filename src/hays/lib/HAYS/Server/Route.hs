module HAYS.Server.Route
    ( Action (..)
    , Route
    , find
    ) where

import           Control.Monad        (join, msum)
import           HAYS.Server.Request  (Request)
import           HAYS.Server.Response (Response)

type Route = Request -> Maybe (Maybe Action)

data Action
  = Impure (IO Action)
  -- ^ Run arbitrary IO.
  | Respond Response
  -- ^ Terminate with a response.

find :: Request -> [Route] -> Maybe Action
find request = msum . map (join . ($ request))
