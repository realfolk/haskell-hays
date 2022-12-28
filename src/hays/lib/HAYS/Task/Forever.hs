{-# LANGUAGE RecordWildCards #-}

module HAYS.Task.Forever
    ( Config
    , OnError
    , Process
    , ToIO
    , defaultConfig
    , forever
    , kill
    , nextError
    , setErrorInterval
    , setLogger
    , setOnError
    , setToIO
    , waitUntilTerminated
    ) where

import qualified Control.Concurrent      as Concurrent
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception       (AsyncException, SomeException)
import qualified Control.Exception       as Exception
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Text               (Text)
import           HAYS.Logger             (Logger)
import           HAYS.Task               (TaskT)
import qualified HAYS.Task               as Task
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

waitUntilTerminated :: Process error' -> IO ()
waitUntilTerminated process = do
  let lock = _terminationLock process
  Lock.acquire lock
  Lock.release lock

kill :: Process error' -> IO ()
kill = Concurrent.killThread . _threadId

nextError :: Process error' -> IO error'
nextError = Chan.readChan . _errorChan

-- * Config

data Config taskConfig error' m a
  = Config
      { _logger        :: Logger
      , _errorInterval :: Time.Seconds
      , _toIO          :: ToIO taskConfig m a
      , _onError       :: OnError taskConfig error'
      }

-- ** Constructors

defaultConfig :: Text -> ToIO taskConfig m a -> Config taskConfig error' m a
defaultConfig name toIO =
  Config (Task.defaultLogger name) 0 toIO (const id)

-- ** Setters

setLogger :: Logger -> Config taskConfig error' m a -> Config taskConfig error' m a
setLogger logger config = config { _logger = logger }

setErrorInterval :: Time.Seconds -> Config taskConfig error' m a -> Config taskConfig error' m a
setErrorInterval errorInterval config = config { _errorInterval = errorInterval }

setToIO :: ToIO taskConfig m a -> Config taskConfig error' m a -> Config taskConfig error' m a
setToIO toIO config = config { _toIO = toIO }

setOnError :: OnError taskConfig error' -> Config taskConfig error' m a -> Config taskConfig error' m a
setOnError onError config = config { _onError = onError }

-- ** ToIO

type ToIO taskConfig m a = taskConfig -> m a -> IO a

-- ** OnError

type OnError taskConfig error' = error' -> taskConfig -> taskConfig

-- * Execution

forever
  :: (MonadIO m)
  => Config taskConfig error' m (Either error' a)
  -> taskConfig
  -> TaskT taskConfig error' m a
  -> IO (Process error')
forever (Config {..}) initialTaskConfig task = do
  errorChan <- Chan.newChan
  terminationLock <- Lock.newAcquired
  threadId <- Concurrent.forkIOWithUnmask $ \unmask -> do
    loopTaskAndCatchExceptions unmask errorChan initialTaskConfig
    Lock.release terminationLock
  return $ Process terminationLock threadId errorChan
    where
      errorIntervalMicroseconds = fromIntegral $ Time.toMicroseconds $ Time.fromSeconds _errorInterval
      runTask taskConfig = _toIO taskConfig $ Task.runTaskT _logger taskConfig task
      loopTaskAndCatchExceptions unmask errorChan taskConfig =
        Exception.catches
          (unmask $ loopTask errorChan taskConfig)
          (makeExceptionHandlers unmask errorChan taskConfig)
      loopTask errorChan taskConfig = do
        outcome <- runTask taskConfig
        case outcome of
          Right _     -> loopTask errorChan taskConfig
          Left error' -> do
            Chan.writeChan errorChan error'
            let newTaskConfig = _onError error' taskConfig
            loopTask errorChan newTaskConfig
      makeExceptionHandlers unmask errorChan taskConfig =
        let
          terminate = return ()
          restart = do
            Concurrent.threadDelay errorIntervalMicroseconds
            loopTaskAndCatchExceptions unmask errorChan taskConfig
        in
        -- Mask 'AsyncException's and terminate the task.
        [ Exception.Handler $
            (\(e :: AsyncException) -> terminate)
        -- Mask all other exceptions and restart the task.
        , Exception.Handler $
            (\(e :: SomeException) -> restart)
        ]
