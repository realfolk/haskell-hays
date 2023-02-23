{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception      (Exception)
import qualified Control.Exception      as Exception
import qualified Control.Monad.IO.Class as Monad.IO
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import           Data.Function          ((&))
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text.Encoding
import           HAYS.Logger            (Logger)
import qualified HAYS.Logger            as Logger
import qualified HAYS.Server            as Server
import           HAYS.Server.Response   (Response)
import qualified HAYS.Server.Response   as Response
import           HAYS.Server.Router     (Router)
import qualified HAYS.Server.Router     as Router
import           HAYS.Task              (TaskT)
import qualified HAYS.Task              as Task
import qualified HAYS.Task.Forever      as Task.Forever
import           Lib.UUID               (UUID)
import qualified Network.HTTP.Types     as HTTP

serverTaskName = "server"

main =
  Task.Forever.run config print () server
    where
      config =
        Task.Forever.defaultConfig (\_ io -> io) (const ())
          & Task.Forever.setLogger (taskLogger serverTaskName Nothing)

-- * Test Exceptions

data TestException = TestException deriving (Show)

instance Exception TestException

-- * Task

type Config = ()

type Error = ()

server :: TaskT Config Error IO ()
server = do
  Task.info ["starting server"]
  Task.warn ["starting server"]
  Task.error' ["starting server"]
  Task.debug ["starting server"]
  Server.defaultServer
    & Server.setLogger (taskLogger serverTaskName . Just)
    & Server.setRouter router
    & Server.setOnMsg onMsg
    & Server.listen
    & Monad.IO.liftIO

router :: Router Msg
router = Router.combine
  [ Router.isPathEnd >> return Home
  , do
    Router.nextPathSectionIs (== "posts")
    blogMsg <- blogRouter
    return $ Blog blogMsg
  , return NotFound
  ]

data Msg
  = Home
  | Blog BlogMsg
  | NotFound

onMsg :: Msg -> IO Response
onMsg msg =
  case msg of
    Home ->
      return $ Response.new HTTP.status200 [] "Hello, home page!"
    Blog blogMsg ->
      onBlogMsg blogMsg
    NotFound ->
      return $ Response.new HTTP.status404 [] "Not Found"

data BlogMsg
  = CreatePost
  | GetAllPosts
  | GetPost Text

blogRouter :: Router BlogMsg
blogRouter =
  Router.combine
    [ do
      Router.nextPathSectionIs (== "create")
      Router.isPathEnd
      return CreatePost
    , Router.isPathEnd >> return GetAllPosts
    , do
      id' <- Router.getNextPathSection
      Router.isPathEnd
      return $ GetPost id'
    ]

onBlogMsg :: BlogMsg -> IO Response
onBlogMsg msg =
  case msg of
    CreatePost ->
      return $ Response.new HTTP.status200 [] "CreatePost"
    GetAllPosts ->
      return $ Response.new HTTP.status200 [] "GetAllPosts"
    GetPost id' ->
      return $ Response.new HTTP.status200 [] $ "GetPost " <> ByteString.Lazy.fromStrict (Text.Encoding.encodeUtf8 id')


-- ** Helpers

taskLogger :: Text -> Maybe UUID -> Logger
taskLogger name maybeRequestID =
  Logger.defaultNamespace sections Logger.terminal
    where
      sections =
        case maybeRequestID of
          Nothing        -> [Logger.plain name]
          Just requestID -> [Logger.plain name, Logger.fromUUID requestID]
