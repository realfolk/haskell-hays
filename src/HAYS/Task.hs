module HAYS.Task
    ( Except.catchError
    , Except.liftEither
    , Except.throwError
    , Reader.ask
    , Reader.asks
    , Reader.liftIO
    , Reader.local
    , TaskT
    , Trans.lift
    , debug
    , error'
    , getLogger
    , info
    , localLogger
    , runTaskT
    , warn
    ) where

import           Control.Monad.Except   (ExceptT, MonadError)
import qualified Control.Monad.Except   as Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import qualified Control.Monad.Reader   as Reader
import           Control.Monad.Trans    (MonadTrans)
import qualified Control.Monad.Trans    as Trans
import           Data.Text              (Text)
import           HAYS.Logger            (Logger)
import qualified HAYS.Logger            as Logger

-- * TaskT

newtype TaskT config error' m a
  = TaskT (ReaderT (Environment config) (ExceptT error' m) a)
  deriving (Applicative, Functor, Monad, MonadError error', MonadIO)

instance MonadTrans (TaskT config error') where
  lift = TaskT . Trans.lift . Trans.lift

-- | Only want users to access @config@ via 'TaskT' 'MonadReader' instance.
instance Monad m => MonadReader config (TaskT config error' m) where
  ask = TaskT $ do
    (Environment config _) <- Reader.ask
    return config
  local f (TaskT m) = TaskT $
    Reader.local
      (\(Environment config logger) -> Environment (f config) logger)
      m

-- ** Execution

runTaskT :: Monad m => Logger -> config -> TaskT config error' m a -> m (Either error' a)
runTaskT logger config (TaskT m) = do
  let environment = Environment config logger
  Except.runExceptT (Reader.runReaderT m environment)

-- ** Logger

info :: (MonadIO m) => Logger.Record -> TaskT config error' m ()
info = log' Logger.info

warn :: (MonadIO m) => Logger.Record -> TaskT config error' m ()
warn = log' Logger.warn

error' :: (MonadIO m) => Logger.Record -> TaskT config error' m ()
error' = log' Logger.error'

debug :: (MonadIO m) => Logger.Record -> TaskT config error' m ()
debug = log' Logger.debug

getLogger :: Monad m => TaskT config error' m Logger
getLogger = TaskT $ do
  (Environment _ logger) <- Reader.ask
  return logger

localLogger :: Monad m => (Logger -> Logger) -> TaskT config error' m a -> TaskT config error' m a
localLogger modifyLogger (TaskT m) = TaskT $ Reader.local modifyEnvironment m
  where
    modifyEnvironment (Environment config logger) =
      Environment config (modifyLogger logger)

-- ** Internal

log' :: (MonadIO m)  => (Logger -> Logger.Record -> IO ()) -> Logger.Record -> TaskT config error' m ()
log' getLogFunction records = do
  logger <- getLogger
  Reader.liftIO $ getLogFunction logger records

-- * Environment

data Environment config
  = Environment config Logger
