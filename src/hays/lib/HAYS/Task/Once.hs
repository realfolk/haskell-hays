{-# LANGUAGE RecordWildCards #-}

module HAYS.Task.Once
    ( Config
    , Exception.Handler (..)
    , Process
    , ToIO
    , defaultConfig
    , fork
    , kill
    , newConfig
    , run
    , setExceptionHandlers
    , setLogger
    , setToIO
    , wait
    ) where

import qualified Control.Concurrent     as Concurrent
import           Control.Exception      (SomeException)
import qualified Control.Exception      as Exception
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as Monad.IO
import           HAYS.Logger            (Logger)
import           HAYS.Task              (TaskT)
import qualified HAYS.Task              as Task
import           HAYS.Task.Types        (ToIO)

-- * Process

data Process a
  = Process
      { _threadId   :: Concurrent.ThreadId
      , _resultLock :: Concurrent.MVar a
      }

wait :: MonadIO m => Process a -> m a
wait (Process {..}) =
  Monad.IO.liftIO $
    Concurrent.readMVar _resultLock

kill :: MonadIO m => Process error' -> m ()
kill = Monad.IO.liftIO . Concurrent.killThread . _threadId

-- * Config

data Config taskConfig error' m a
  = Config
      { _logger            :: Logger
      , _toIO              :: ToIO taskConfig m a
      , _exceptionHandlers :: [Exception.Handler error']
      }

-- ** Constructors

newConfig
  :: Logger
  -> ToIO taskConfig m a
  -> [Exception.Handler error']
  -> Config taskConfig error' m a
newConfig = Config

defaultConfig
  :: ToIO taskConfig m a
  -> (SomeException -> error')
  -> Config taskConfig error' m a
defaultConfig toIO onSomeException =
  newConfig
    Task.defaultLogger
    toIO
    [ Exception.Handler (return . onSomeException)
    ]

-- ** Setters

setLogger :: Logger -> Config taskConfig error' m a -> Config taskConfig error' m a
setLogger logger config = config { _logger = logger }

setToIO :: ToIO taskConfig m a -> Config taskConfig error' m a -> Config taskConfig error' m a
setToIO toIO config = config { _toIO = toIO }

setExceptionHandlers :: [Exception.Handler error'] -> Config taskConfig error' m a -> Config taskConfig error' m a
setExceptionHandlers exceptionHandlers config = config { _exceptionHandlers = exceptionHandlers }

-- * Execution

run
  :: (Monad m, MonadIO n)
  => Config taskConfig error' m (Either error' a)
  -> taskConfig
  -> TaskT taskConfig error' m a
  -> n (Either error' a)
run onceConfig taskConfig task =
  Monad.IO.liftIO $ Exception.bracket acquire release action
    where
      acquire = fork onceConfig taskConfig task
      release = kill
      action = wait

fork
  :: (Monad m, MonadIO n)
  => Config taskConfig error' m (Either error' a)
  -> taskConfig
  -> TaskT taskConfig error' m a
  -> n (Process (Either error' a))
fork (Config {..}) taskConfig task = Monad.IO.liftIO $ do
  resultLock <- Concurrent.newEmptyMVar
  threadId <- Concurrent.forkIO $ do
    let executeTask = _toIO taskConfig $ Task.runTaskT _logger taskConfig task
    result <- Exception.catches executeTask (map (fmap Left) _exceptionHandlers)
    Concurrent.putMVar resultLock result
  return $ Process threadId resultLock
