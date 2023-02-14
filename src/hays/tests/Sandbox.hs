{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import qualified HAYS.Task.Forever      as Task
import           Lib.UUID               (UUID)
import qualified Network.HTTP.Types     as HTTP

serverTaskName = "server"

main =
  Exception.bracket (Task.forever config () server) Task.kill Task.waitUntilTerminated
    where
      config =
        Task.defaultConfig serverTaskName (\_ io -> io)
          & Task.setLogger (taskLogger serverTaskName Nothing)
          & Task.setErrorInterval 12

-- * Task

type Config = ()

type Error = ()

type ServerM a = TaskT Config Error IO a

server :: ServerM ()
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
