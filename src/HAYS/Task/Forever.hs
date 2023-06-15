{-# LANGUAGE RecordWildCards #-}

module HAYS.Task.Forever
    ( Config
    , IterationOutcome
    , Process
    , ToIO
    , defaultConfig
    , delayedLoop
    , fork
    , getNextError
    , kill
    , loop
    , newConfig
    , run
    , setExceptionHandlers
    , setLogger
    , setOnIterationResult
    , setToIO
    , terminate
    , updateTaskConfigAndDelayedLoop
    , updateTaskConfigAndLoop
    , wait
    ) where

import qualified Control.Concurrent      as Concurrent
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception       (SomeException)
import qualified Control.Exception       as Exception
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO)
import qualified Control.Monad.IO.Class  as Monad.IO
import           HAYS.Logger             (Logger)
import           HAYS.Task               (TaskT)
import qualified HAYS.Task               as Task
import qualified HAYS.Task.Once          as Task.Once
import           HAYS.Task.Types         (ToIO)
import           Pouch.Concurrent.Lock     (Lock)
import qualified Pouch.Concurrent.Lock     as Lock
import qualified Pouch.Time                as Time

-- * Process

data Process error'
  = Process
      { _terminationLock :: Lock
      , _threadId        :: Concurrent.ThreadId
      , _errorChan       :: Chan error'
      }

wait :: MonadIO m => Process error' -> m ()
wait (Process {..}) =
  Monad.IO.liftIO $
    Lock.acquire _terminationLock >>
      Lock.release _terminationLock

kill :: MonadIO m => Process error' -> m ()
kill = Monad.IO.liftIO . Concurrent.killThread . _threadId

-- TODO how to handle when the process terminates?
-- Currently, it would hang indefinitely.
getNextError :: Process error' -> IO error'
getNextError = Chan.readChan . _errorChan

-- * Config

data Config taskConfig error' m a
  = Config
      { _logger :: Logger
      , _toIO :: ToIO taskConfig m (Either error' a)
      , _onIterationResult :: taskConfig -> Either error' a -> IterationOutcome taskConfig
      , _exceptionHandlers :: [Exception.Handler error']
      }

-- ** Constructors

newConfig
  :: Logger
  -> ToIO taskConfig m (Either error' a)
  -> (taskConfig -> Either error' a -> IterationOutcome taskConfig)
  -> [Exception.Handler error']
  -> Config taskConfig error' m a
newConfig = Config

defaultConfig
  :: Logger
  -> ToIO taskConfig m (Either error' a)
  -> (SomeException -> error')
  -> Config taskConfig error' m a
defaultConfig logger toIO onSomeException =
  newConfig
    logger
    toIO
    (const (either (const terminate) (const loop)))
    [ Exception.Handler (return . onSomeException)
    ]

-- ** Setters

setLogger :: Logger -> Config taskConfig error' m a -> Config taskConfig error' m a
setLogger logger config = config { _logger = logger }

setToIO :: ToIO taskConfig m (Either error' a) -> Config taskConfig error' m a -> Config taskConfig error' m a
setToIO toIO config = config { _toIO = toIO }

setOnIterationResult :: (taskConfig -> Either error' a -> IterationOutcome taskConfig) -> Config taskConfig error' m a -> Config taskConfig error' m a
setOnIterationResult onIterationResult config = config { _onIterationResult = onIterationResult }

setExceptionHandlers :: [Exception.Handler error'] -> Config taskConfig error' m a -> Config taskConfig error' m a
setExceptionHandlers exceptionHandlers config = config { _exceptionHandlers = exceptionHandlers }

-- * IterationOutcome

data IterationOutcome taskConfig
  = Terminate
  | Loop Time.Milliseconds (taskConfig -> taskConfig)

terminate :: IterationOutcome taskConfig
terminate = Terminate

loop :: IterationOutcome taskConfig
loop = Loop 0 id

delayedLoop :: Time.Milliseconds -> IterationOutcome taskConfig
delayedLoop = flip Loop id

updateTaskConfigAndLoop :: taskConfig -> IterationOutcome taskConfig
updateTaskConfigAndLoop = Loop 0 . const

updateTaskConfigAndDelayedLoop :: Time.Milliseconds -> taskConfig -> IterationOutcome taskConfig
updateTaskConfigAndDelayedLoop interval taskConfig = Loop interval (const taskConfig)

-- * Execution

run
  :: (Monad m, MonadIO n)
  => Config taskConfig error' m a
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
  => Config taskConfig error' m a
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
    onceConfig =
      Task.Once.newConfig _logger _toIO _exceptionHandlers
    loop errorChan taskConfig = do
      -- Run the task.
      result <- Task.Once.run onceConfig taskConfig task
      -- Broadcast any errors on the errorChan.
      either (Chan.writeChan errorChan) (const (return ())) result
      -- Handle the IterationOutcome.
      let iterationOutcome = _onIterationResult taskConfig result
      case iterationOutcome of
        Terminate ->
          return ()
        Loop interval updateTaskConfig -> do
          let intervalMicroseconds = fromIntegral $ Time.toMicroseconds $ Time.fromMilliseconds interval
          when (interval > 0) $ Concurrent.threadDelay intervalMicroseconds
          let newTaskConfig = updateTaskConfig taskConfig
          loop errorChan newTaskConfig
