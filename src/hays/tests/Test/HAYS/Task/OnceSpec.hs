{-# LANGUAGE OverloadedStrings #-}

module Test.HAYS.Task.OnceSpec
    ( spec
    ) where

import qualified Control.Concurrent as Concurrent
import           Control.Exception  (Exception)
import qualified Control.Exception  as Exception
import qualified Data.Bifunctor     as Bifunctor
import qualified HAYS.Logger        as Logger
import           HAYS.Task          (TaskT)
import qualified HAYS.Task          as Task
import qualified HAYS.Task.Once     as Task.Once
import qualified Lib.Time           as Time
import           Test.Hspec

data TaskConfig = TaskConfig deriving (Eq, Show)

data TestException = TestException deriving (Eq, Show)

instance Exception TestException

data Error
  = Error0
  | Error1
  | Exception0 !TestException
  | Exception1 !TestException
  deriving (Eq, Show)

type TaskIO a = TaskT TaskConfig Error IO a

onceConfig :: Task.Once.Config TaskConfig Error IO a
onceConfig =
  Task.Once.newConfig
    Logger.silent
    (const id)
    [Exception.Handler (return . Exception0)]

runTaskIO :: TaskIO a -> IO (Either Error a)
runTaskIO = Task.Once.run onceConfig TaskConfig

forkTaskIO :: TaskIO a -> IO (Task.Once.Process (Either Error a))
forkTaskIO = Task.Once.fork onceConfig TaskConfig

delayMs :: Time.Milliseconds
delayMs = 100

delay :: TaskIO ()
delay =
  Task.liftIO $
    Concurrent.threadDelay $
      fromIntegral $
        Time.toMicroseconds $
          Time.fromMilliseconds delayMs

spec :: Spec
spec = do
  describe "Process" $ do
    describe "wait" $ do
      context "when forking a Task" $ do
        it "blocks the thread until the forked Task completes execution" $ do
          before <- Time.now
          process <- forkTaskIO delay
          Task.Once.wait process
          after <- Time.now
          after - before `shouldSatisfy` (>= Time.fromMilliseconds delayMs)
        it "allows reading the forked Task's result multiple times" $ do
          process <- forkTaskIO $ return 0
          result0 <- Task.Once.wait process
          result1 <- Task.Once.wait process
          result2 <- Task.Once.wait process
          (result0, result1, result2) `shouldBe` (Right 0, Right 0, Right 0)
    describe "kill" $ do
      context "when forking a Task" $ do
        it "stops the forked Task immediately" $ do
          before <- Time.now
          process <- forkTaskIO delay
          Task.Once.kill process
          after <- Time.now
          after - before `shouldSatisfy` (<= Time.fromMilliseconds delayMs)
  describe "Config" $ do
    describe "setLogger" $ do
      it "correctly sets the logger" $ do
        mVar <- Concurrent.newEmptyMVar
        let logger = Logger.fromIO (curry (Concurrent.putMVar mVar . Bifunctor.second Logger.toPlainText))
        let config = Task.Once.setLogger logger onceConfig
        Task.Once.run config TaskConfig $ Task.info "message"
        Concurrent.readMVar mVar `shouldReturn` (Logger.Info, "message")
    describe "setToIO" $ do
      it "correctly sets the toIO function" $ do
        mVar <- Concurrent.newEmptyMVar
        let config = Task.Once.setToIO (\_ m -> Concurrent.putMVar mVar 0 >> m) onceConfig
        Task.Once.run config TaskConfig $ return ()
        Concurrent.readMVar mVar `shouldReturn` 0
    describe "setExceptionHandlers" $ do
      it "correctly sets the exceptionHandlers" $ do
        let config = Task.Once.setExceptionHandlers [Exception.Handler $ return . Exception1] onceConfig
        result <- Task.Once.run config TaskConfig $ Exception.throw TestException
        result `shouldBe` (Left (Exception1 TestException) :: Either Error ())
  describe "Execution" $ do
    describe "run" $ do
      context "when the Task successfully executes" $ do
        it "returns the correct value" $ do
          runTaskIO (return 0) `shouldReturn` Right 0
      context "when an error is thrown during the execution of the Task" $ do
        it "returns the correct value" $ do
          runTaskIO (Task.throwError Error0) `shouldReturn` (Left Error0 :: Either Error ())
      context "when an exception is raised during the execution of the Task" $ do
        it "returns the correct value" $ do
          runTaskIO (Exception.throw TestException) `shouldReturn` (Left (Exception0 TestException) :: Either Error ())
    describe "fork" $ do
      context "when the Task successfully executes" $ do
        context "when waiting for the forked Process to complete" $ do
          it "returns the correct value" $ do
            process <- forkTaskIO $ return 0
            Task.Once.wait process `shouldReturn` Right 0
      context "when an error is thrown during the execution of the Task" $ do
        context "when waiting for the forked Process to complete" $ do
          it "returns the correct value" $ do
            process <- forkTaskIO $ Task.throwError Error0
            Task.Once.wait process `shouldReturn` (Left Error0 :: Either Error ())
      context "when an exception is raised during the execution of the Task" $ do
        context "when waiting for the forked Process to complete" $ do
          it "returns the correct value" $ do
            process <- forkTaskIO $ Exception.throw TestException
            Task.Once.wait process `shouldReturn` (Left (Exception0 TestException) :: Either Error ())
