{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception      as Exception
import qualified Control.Monad.IO.Class as Monad.IO
import           Data.Function          ((&))
import           Data.Text              (Text)
import           HAYS.Logger            (Logger)
import qualified HAYS.Logger            as Logger
import qualified HAYS.Server            as Server
import           HAYS.Task              (TaskT)
import qualified HAYS.Task              as Task
import qualified HAYS.Task.Forever      as Task
import           Lib.UUID               (UUID)

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
    & Server.listen
    & Monad.IO.liftIO

-- ** Helpers

taskLogger :: Text -> Maybe UUID -> Logger
taskLogger name maybeRequestID =
  Logger.defaultNamespace sections Logger.terminal
    where
      sections =
        case maybeRequestID of
          Nothing        -> [Logger.plain name]
          Just requestID -> [Logger.plain name, Logger.fromUUID requestID]
