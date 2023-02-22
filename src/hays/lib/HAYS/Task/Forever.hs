{-# LANGUAGE RecordWildCards #-}

module HAYS.Task.Forever
    ( Config
    , OnError
    , Process
    , ToIO
    , defaultConfig
    , fork
    , getNextError
    , kill
    , newConfig
    , run
    , setErrorInterval
    , setExceptionHandlers
    , setLogger
    , setMapConfigOnError
    , setToIO
    , wait
    ) where

import qualified Control.Concurrent      as Concurrent
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception       (SomeException)
import qualified Control.Exception       as Exception
import           Control.Monad.IO.Class  (MonadIO)
import qualified Control.Monad.IO.Class  as Monad.IO
import           HAYS.Logger             (Logger)
import           HAYS.Task               (TaskT)
import qualified HAYS.Task               as Task
import qualified HAYS.Task.Once          as Task.Once
import           HAYS.Task.Types         (OnError, ToIO)
import           Lib.Concurrent.Lock     (Lock)
import qualified Lib.Concurrent.Lock     as Lock
import qualified Lib.Time                as Time

-- * Process

data Process error'
  = Process
      { _terminationLock :: Lock
      , _threadId        :: Concurrent.ThreadId
      , _errorChan       :: Chan error'
      }

wait :: MonadIO m => Process error' -> m ()
wait process = Monad.IO.liftIO $ do
  let lock = _terminationLock process
  Lock.acquire lock
  Lock.release lock

kill :: MonadIO m => Process error' -> m ()
kill = Monad.IO.liftIO . Concurrent.killThread . _threadId

getNextError :: Process error' -> IO error'
getNextError = Chan.readChan . _errorChan

-- * Config

data Config taskConfig error' m a
  = Config
      { _logger            :: Logger
      , _errorInterval     :: Time.Seconds
      , _toIO              :: ToIO taskConfig m a
      , _mapConfigOnError  :: OnError error' (taskConfig -> taskConfig)
      , _exceptionHandlers :: [Exception.Handler error']
      }

-- ** Constructors

newConfig
  :: Logger
  -> Time.Seconds
  -> ToIO taskConfig m a
  -> OnError error' (taskConfig -> taskConfig)
  -> [Exception.Handler error']
  -> Config taskConfig error' m a
newConfig = Config

defaultConfig
  :: ToIO taskConfig m a
  -> (SomeException -> error')
  -> Config taskConfig error' m a
defaultConfig toIO onSomeException =
  Config
    Task.defaultLogger
    0
    toIO
    (const id)
    [ Exception.Handler (return . onSomeException)
    ]

-- ** Setters

setLogger :: Logger -> Config taskConfig error' m a -> Config taskConfig error' m a
setLogger logger config = config { _logger = logger }

setErrorInterval :: Time.Seconds -> Config taskConfig error' m a -> Config taskConfig error' m a
setErrorInterval errorInterval config = config { _errorInterval = errorInterval }

setToIO :: ToIO taskConfig m a -> Config taskConfig error' m a -> Config taskConfig error' m a
setToIO toIO config = config { _toIO = toIO }

setMapConfigOnError :: OnError error' (taskConfig -> taskConfig) -> Config taskConfig error' m a -> Config taskConfig error' m a
setMapConfigOnError onError config = config { _mapConfigOnError = onError }

setExceptionHandlers :: [Exception.Handler error'] -> Config taskConfig error' m a -> Config taskConfig error' m a
setExceptionHandlers exceptionHandlers config = config { _exceptionHandlers = exceptionHandlers }

-- * Execution

run
  :: (Monad m, MonadIO n)
  => Config taskConfig error' m (Either error' a)
  -> (error' -> IO ())
  -> taskConfig
  -> TaskT taskConfig error' m a
  -> n ()
run foreverConfig handleError taskConfig task =
  Monad.IO.liftIO $ Exception.bracket acquire release action
    where
      acquire = do
        process <- fork foreverConfig taskConfig task
        errorThreadID <- Concurrent.forkIO $ handleErrorLoop $ getNextError process
        return (process, errorThreadID)
      release (process, errorThreadID) = do
        kill process
        Concurrent.killThread errorThreadID
      action (process, _) =
        wait process
      handleErrorLoop getNextError' = do
        error' <- getNextError'
        handleError error'
        handleErrorLoop getNextError'

fork
  :: (Monad m, MonadIO n)
  => Config taskConfig error' m (Either error' a)
  -> taskConfig
  -> TaskT taskConfig error' m a
  -> n (Process error')
fork (Config {..}) initialTaskConfig task =
  Monad.IO.liftIO $ do
    errorChan <- Chan.newChan
    terminationLock <- Lock.newAcquired
    threadId <- Concurrent.forkIO $ do
      loop errorChan initialTaskConfig
      Lock.release terminationLock
    return $ Process terminationLock threadId errorChan
  where
    errorIntervalMicroseconds =
      fromIntegral $ Time.toMicroseconds $ Time.fromSeconds _errorInterval
    onceConfig =
      Task.Once.newConfig _logger _toIO _exceptionHandlers
    loop errorChan taskConfig = do
      result0 <- Task.Once.run onceConfig taskConfig task
      case result0 of
        Left error' -> do
          Chan.writeChan errorChan error'
          Concurrent.threadDelay errorIntervalMicroseconds
          let newTaskConfig = _mapConfigOnError error' taskConfig
          loop errorChan newTaskConfig
        Right _ -> loop errorChan taskConfig
