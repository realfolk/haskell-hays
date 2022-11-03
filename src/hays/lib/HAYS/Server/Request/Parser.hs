{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module HAYS.Server.Request.Parser
    ( Parser
    , anything
    , bodyAny
    , bodyEmpty
    , bodyJSON
    , bodyNonEmpty
    , header
    , headerContains
    , headerContains'
    , headerEquals
    , headerIs
    , headers
    , isAcceptingJSON
    , isProvidingJSON
    , json
    , method
    , pathEnd
    , pathExact
    , pathIs
    , pathParam
    , pathRest
    , queryEnd
    , queryExact
    , queryIs
    , queryParamOptional
    , queryParamRequired
    , queryRest
    , (->>)
    , (=>>)
    ) where

import           Control.Applicative     ((<|>))
import qualified Data.Aeson              as A
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.List               (find, partition)
import           Data.Maybe              (isJust)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           HAYS.Server.Request     (Headers, Method, Path, Query,
                                          QueryItem, Request)
import qualified Lib.URL.Component.Path  as Path
import qualified Lib.URL.Component.Query as Query
import qualified Network.HTTP.Types      as H

type Parser a b = Request -> Maybe (Request, a -> b)

(=>>) :: Parser handler result -> (Request -> handler) -> Request -> Maybe result
(=>>) parser handler req = do
  (_, f) <- parser req
  return $ f $ handler req

(->>) :: Parser a b -> Parser b c -> Parser a c
(->>) pa pb req = do
  (reqA, fa) <- pa req
  (reqB, fb) <- pb reqA
  return (reqB, fb . fa)

(<->) :: Parser a b -> Parser a b -> Parser a b
(<->) pa pb req = pa req <|> pb req

invert :: Parser a b -> Parser a a
invert p req =
  case p req of
    Nothing -> Just (req, id)
    Just _  -> Nothing

-- *** Request Parser Helpers
-- | An identity parser
anything :: Parser a a
anything req = Just (req, id)

-- | Determines whether the given method matches the method used in the parser's request.
method :: Method -> Parser a a
method m req@(m', _, _, _, _) =
  if m == m'
    then Just (req, id)
    else Nothing

pathEnd :: Parser a a
pathEnd req@(_, [], _, _, _) = Just (req, id)
-- Support trailing slashes (last path fragment is empty).
pathEnd req@(_, [last], _, _, _)
  | last == T.empty = Just (req, id)
pathEnd _ = Nothing

pathIs :: (T.Text -> Bool) -> Parser (T.Text -> a) a
pathIs f (m, p, q, h, b) = do
  (nextPathSection, remainingPath) <- Path.uncons p
  if f nextPathSection
     then Just ((m, remainingPath, q, h, b), \f -> f nextPathSection)
     else Nothing

pathExact :: T.Text -> Parser a a
pathExact t req = do
  (req', _) <- pathIs (== t) req
  return (req', id)

pathParam :: Parser (T.Text -> a) a
pathParam req = do
  (req', f) <- pathIs (const True) req
  return (req', \g -> g $ f id)

pathRest :: Parser (Path -> a) a
pathRest (m, ps, q, h, b) = Just ((m, [], q, h, b), \f -> f ps)

queryRest :: Parser (Query -> a) a
queryRest (m, ps, q, h , b) = Just ((m, ps, [], h, b), \f -> f q)

queryEnd :: Parser a a
queryEnd req@(_, _, [], _, _) = Just (req, id)
queryEnd _                    = Nothing

queryIs :: (QueryItem -> Bool) -> Parser (QueryItem -> a) a
queryIs f (m, p, q, h, b) =
  case Query.findItem f q of
    (Just queryItem, nonMatches) ->
      Just ((m, p, nonMatches, h, b), \f -> f queryItem)
    _ -> Nothing

-- | Indicates if a value's name matches the provided text exactly.
queryExact ::
  T.Text -- ^ Exact text to match against.
  -> T.Text -- ^ Contains the value to match with.
  -> Parser a a -- ^ Returns the parsed request if matches, Nothing otherwise.
queryExact name value req = do
  (req', _) <- queryIs (== (name, value)) req
  return (req', id)

-- | Attempts to match a query parameter in the given request against the provided text.
--This might perform very slowly given the number of times the list
--is iterated on.
queryParamRequired ::
  T.Text -- ^ Text to match the query parameter against.
  -> Parser (T.Text -> a) a -- ^ Returns the request with remaining, non-matched query parameters.
queryParamRequired name (m, p, q, h, b) = do
  (_, value) <- maybeMatch
  return ((m, p, nonMatched, h, b), \f -> f value)
  where
    predicate (n, v) = n == name && not (T.null v)
    (maybeMatch, nonMatched) = Query.findItem predicate q

queryParamOptional ::
     T.Text -> Parser (Maybe T.Text -> a) a
queryParamOptional name = required <-> (\req -> Just (req, \f -> f Nothing))
  where
    required req = do
      (req', f) <- queryParamRequired name req
      return (req', \g -> g (Just (f id)))

header :: H.HeaderName -> Parser (BS.ByteString -> a) a
header name (m, p, q, h, b) =
  case partition ((== name) . fst) h of
    ([], _) -> Nothing
    ((_, value):otherMatches, nonMatches) ->
      Just ((m, p, q, otherMatches ++ nonMatches, b), \f -> f value)

headers :: Parser (Headers -> a) a
headers (m, p, q, h, b) = Just ((m, p, q, [], b), \f -> f h)

headerIs :: H.HeaderName -> (BS.ByteString -> Bool) -> Parser a a
headerIs name fn (m, p, q, h, b) =
  case partition (\(n, v) -> n == name && fn v) h of
    ([], _) -> Nothing
    (_:otherMatches, nonMatches) ->
      Just ((m, p, q, otherMatches ++ nonMatches, b), id)

headerEquals :: H.HeaderName -> BS.ByteString -> Parser a a
headerEquals name value = headerIs name (== value)

headerContains :: H.HeaderName -> BS.ByteString -> Parser a a
headerContains name value = headerIs name (BS.isInfixOf value)

isProvidingJSON :: Parser a a
isProvidingJSON = headerContains H.hContentType jsonMimeType

-- | Indicates if the server is accepting JSON messages.
isAcceptingJSON :: Parser a a
isAcceptingJSON = headerContains H.hAccept jsonMimeType

jsonMimeType = "application/json"

-- | @bodyJSON encoding@ succeeds when an incoming request's body is able to be decoded using 'A.FromJSON', providing the decoded body to the route handler. This 'Parser' implies 'isProvidingJSON'.
bodyJSON :: A.FromJSON b => Parser (b -> a) a
bodyJSON =
  isProvidingJSON ->> \(m, p, q, h, b) -> do
    decoded <- A.decode b
    return ((m, p, q, h, LBS.empty), \f -> f decoded)

-- | @json encoding@ is a combination of 'isAcceptingJSON' and 'bodyJSON'.
json :: A.FromJSON b => Parser (b -> a) a
json = isAcceptingJSON ->> bodyJSON

bodyAny :: Parser (LBS.ByteString -> a) a
bodyAny (m, p, q, h, b) = Just ((m, p, q, h, LBS.empty), \f -> f b)

bodyEmpty :: Parser a a
bodyEmpty (m, p, q, h, b)
  | b == LBS.empty = Just ((m, p, q, h, b), id)
  | otherwise = Nothing

bodyNonEmpty :: Parser (LBS.ByteString -> a) a
bodyNonEmpty (m, p, q, h, b)
  | b == LBS.empty = Nothing
  | otherwise = Just ((m, p, q, h, LBS.empty), \f -> f b)

-- *** Misc. Helpers
headerIs' :: H.HeaderName -> (BS.ByteString -> Bool) -> Headers -> Bool
headerIs' name fn = isJust . find (\(n, v) -> n == name && fn v)

headerContains' :: H.HeaderName -> BS.ByteString -> Headers -> Bool
headerContains' name value = headerIs' name (BS.isInfixOf value)
