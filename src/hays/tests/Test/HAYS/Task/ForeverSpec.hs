{-# LANGUAGE OverloadedStrings #-}

module Test.HAYS.Task.ForeverSpec
    ( spec
    ) where

import qualified Control.Concurrent as Concurrent
import           Control.Exception  (Exception)
import qualified Control.Exception  as Exception
import qualified HAYS.Logger        as Logger
import           HAYS.Task          (TaskT)
import qualified HAYS.Task          as Task
import qualified HAYS.Task.Forever  as Task.Forever
import qualified Lib.Time           as Time
import           Test.Hspec

data TaskConfig = TaskConfig0 | TaskConfig1 deriving (Eq, Show)

data TestException = TestException deriving (Eq, Show)

instance Exception TestException

data Error
  = Error0
  | Error1
  | Exception0 !TestException
  | Exception1 !TestException
  | Terminate -- Throw 'Terminate' to terminate a running Task.
  deriving (Eq, Show)

type TaskIO a = TaskT TaskConfig Error IO a

foreverConfig :: Task.Forever.Config TaskConfig Error IO a
foreverConfig =
  Task.Forever.newConfig
    Task.defaultLogger
    (const id)
    defaultOnIterationResult
    [ Exception.Handler (return . Exception0)
    ]

defaultOnIterationResult :: TaskConfig -> Either Error a -> Task.Forever.IterationOutcome TaskConfig
defaultOnIterationResult _ result =
  case result of
    Left Terminate -> Task.Forever.terminate
    _              -> Task.Forever.loop

runTaskIO :: TaskIO a -> IO ()
runTaskIO = Task.Forever.run foreverConfig (const (return ())) TaskConfig0

forkTaskIO :: TaskIO a -> IO (Task.Forever.Process Error)
forkTaskIO = Task.Forever.fork foreverConfig TaskConfig0

terminate :: TaskIO ()
terminate = Task.throwError Terminate

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
        it "blocks the thread until the forked Task terminates" $ do
          before <- Time.now
          process <- forkTaskIO $ delay >> terminate
          Task.Forever.wait process
          after <- Time.now
          after - before `shouldSatisfy` (>= Time.fromMilliseconds delayMs)
    describe "kill" $ do
      context "when forking a Task" $ do
        it "stops the forked Task immediately" $ do
          before <- Time.now
          process <- forkTaskIO delay
          Task.Forever.kill process
          after <- Time.now
          after - before `shouldSatisfy` (<= Time.fromMilliseconds delayMs)
    describe "getNextError" $ do
      context "when forking a Task that throws an error across multiple iterations" $ do
        it "successfully gets the errors" $ do
          process <- forkTaskIO $ do
            Task.throwError Error0
          error0 <- Task.Forever.getNextError process
          error1 <- Task.Forever.getNextError process
          error2 <- Task.Forever.getNextError process
          Task.Forever.kill process
          (error0, error1, error2) `shouldBe` (Error0, Error0, Error0)
  describe "Config" $ do
    describe "setLogger" $ do
      it "correctly sets the logger" $ do
        mVar <- Concurrent.newEmptyMVar
        let logger = Logger.fromIO (curry (Concurrent.putMVar mVar))
        let config = Task.Forever.setLogger logger foreverConfig
        Task.Forever.fork config TaskConfig0 $ Task.info "message" >> terminate
        Concurrent.readMVar mVar `shouldReturn` (Logger.Info, "message")
    describe "setToIO" $ do
      it "correctly sets the toIO function" $ do
        mVar <- Concurrent.newEmptyMVar
        let config = Task.Forever.setToIO (\_ m -> Concurrent.putMVar mVar 0 >> m) foreverConfig
        Task.Forever.fork config TaskConfig0 terminate
        Concurrent.readMVar mVar `shouldReturn` 0
    describe "setOnIterationResult" $ do
      it "correctly sets the onIterationResult function" $ do
        mVar <- Concurrent.newMVar 0
        let config = Task.Forever.setOnIterationResult (\_ _ -> Task.Forever.terminate) foreverConfig
        process <- Task.Forever.fork config TaskConfig0 $
          Task.liftIO $
            Concurrent.modifyMVar_ mVar (return . (+ 1))
        Task.Forever.wait process
        Concurrent.readMVar mVar `shouldReturn` 1
      context "when the IterationOutcome is to terminate" $ do
        it "terminates the Process without further iterations" $ do
          mVar <- Concurrent.newMVar 0
          let config = Task.Forever.setOnIterationResult (\_ _ -> Task.Forever.terminate) foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $
            Task.liftIO $
              Concurrent.modifyMVar_ mVar (return . (+ 1))
          Task.Forever.wait process
          Concurrent.readMVar mVar `shouldReturn` 1
      context "when the IterationOutcome is to loop" $ do
        it "loops the Task indefinitely until terminated without delay" $ do
          before <- Time.now
          let iterations = 10
          mVar <- Concurrent.newMVar iterations
          let onIterationResult _ result = case result of
                                             Left Terminate -> Task.Forever.terminate
                                             _ -> Task.Forever.loop
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $ do
            iterationCount <- Task.liftIO $ Concurrent.takeMVar mVar
            if iterationCount == 0
               then terminate
               else Task.liftIO $ Concurrent.putMVar mVar (iterationCount - 1)
          Task.Forever.wait process
          after <- Time.now
          after - before `shouldSatisfy` (<= Time.fromMilliseconds (delayMs * fromIntegral iterations))
      context "when the IterationOutcome is to loop with a delay" $ do
        it "loops the Task indefinitely with a delay between iterations" $ do
          before <- Time.now
          let iterations = 3
          mVar <- Concurrent.newMVar iterations
          let onIterationResult _ result = case result of
                                             Left Terminate -> Task.Forever.terminate
                                             _ -> Task.Forever.delayedLoop delayMs
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $ do
            iterationCount <- Task.liftIO $ Concurrent.takeMVar mVar
            if iterationCount == 0
               then terminate
               else Task.liftIO $ Concurrent.putMVar mVar (iterationCount - 1)
          Task.Forever.wait process
          after <- Time.now
          after - before `shouldSatisfy` (>= Time.fromMilliseconds (delayMs * fromIntegral iterations))
      context "when the IterationOutcome is to update the Task config then loop" $ do
        it "loops the Task with the updated config" $ do
          mVar <- Concurrent.newMVar Nothing
          let onIterationResult config' _ = case config' of
                                              TaskConfig0 -> Task.Forever.updateTaskConfigAndLoop TaskConfig1
                                              TaskConfig1 -> Task.Forever.terminate
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $ do
            config <- Task.ask
            Task.liftIO $ Concurrent.swapMVar mVar $ Just config
          Task.Forever.wait process
          Concurrent.readMVar mVar `shouldReturn` Just TaskConfig1
      context "when the IterationOutcome is to update the task config then loop with a delay" $ do
        it "loops the Task with the updated config with a delay between iterations" $ do
          before <- Time.now
          mVar <- Concurrent.newMVar Nothing
          let onIterationResult config' _ = case config' of
                                              TaskConfig0 -> Task.Forever.updateTaskConfigAndDelayedLoop delayMs TaskConfig1
                                              TaskConfig1 -> Task.Forever.terminate
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $ do
            config <- Task.ask
            Task.liftIO $ Concurrent.swapMVar mVar $ Just config
          Task.Forever.wait process
          after <- Time.now
          after - before `shouldSatisfy` (>= Time.fromMilliseconds delayMs)
          Concurrent.readMVar mVar `shouldReturn` Just TaskConfig1
    describe "setExceptionHandlers" $ do
      it "correctly sets the exceptionHandlers" $ do
        let config = Task.Forever.setExceptionHandlers [Exception.Handler $ return . Exception1] foreverConfig
        process <- Task.Forever.fork config TaskConfig0 $ Exception.throw TestException >> terminate
        Task.Forever.getNextError process `shouldReturn` Exception1 TestException
  describe "Execution" $ do
    describe "run" $ do
      context "when the Task always iterates successfully without issues" $ do
        it "passes the correct value to onIterationResult" $ do
          mVar <- Concurrent.newMVar 0
          let onIterationResult _ result = case result of
                                             Right () -> Task.Forever.loop
                                             _        -> Task.Forever.terminate
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          threadId <- Concurrent.forkIO $
            Task.Forever.run config (const (return ())) TaskConfig0 $
              Task.liftIO $ Concurrent.modifyMVar_ mVar (return . (+ 1))
          Concurrent.threadDelay $ fromIntegral $ Time.toMicroseconds $ Time.fromMilliseconds delayMs
          Concurrent.killThread threadId
          completedIterations <- Concurrent.readMVar mVar
          completedIterations `shouldSatisfy` (> 1)
      context "when an error is thrown during a Task iteration" $ do
        it "passes the correct value to onIterationResult" $ do
          mVar <- Concurrent.newMVar 0
          let onIterationResult _ result = case result of
                                             Left Error0 -> Task.Forever.terminate
                                             _        -> Task.Forever.loop
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          Task.Forever.run config (const (return ())) TaskConfig0 $ do
            Task.liftIO $ Concurrent.modifyMVar_ mVar (return . (+ 1))
            Task.throwError Error0
          Concurrent.readMVar mVar `shouldReturn` 1
        it "passes the correct value to handleError" $ do
          mVar <- Concurrent.newEmptyMVar
          let onIterationResult _ _ = Task.Forever.terminate
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          Task.Forever.run config (Concurrent.putMVar mVar) TaskConfig0 $
            Task.throwError Error0
          Concurrent.readMVar mVar `shouldReturn` Error0
      context "when an exception is raised during the execution of the Task" $ do
        it "passes the correct value to onIterationResult" $ do
          mVar <- Concurrent.newMVar 0
          let onIterationResult _ result = case result of
                                             Left (Exception0 TestException) -> Task.Forever.terminate
                                             _        -> Task.Forever.loop
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          Task.Forever.run config (const (return ())) TaskConfig0 $ do
            Task.liftIO $ Concurrent.modifyMVar_ mVar (return . (+ 1))
            Exception.throw TestException
          Concurrent.readMVar mVar `shouldReturn` 1
        it "passes the correctValue to handleError" $ do
          mVar <- Concurrent.newEmptyMVar
          let onIterationResult _ _ = Task.Forever.terminate
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          Task.Forever.run config (Concurrent.putMVar mVar) TaskConfig0 $
            Exception.throw TestException
          Concurrent.readMVar mVar `shouldReturn` Exception0 TestException
    describe "fork" $ do
      context "when the Task always iterates successfully without issues" $ do
        it "passes the correct value to onIterationResult" $ do
          mVar <- Concurrent.newMVar 0
          let onIterationResult _ result = case result of
                                             Right () -> Task.Forever.loop
                                             _        -> Task.Forever.terminate
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $
            Task.liftIO $ Concurrent.modifyMVar_ mVar (return . (+ 1))
          Concurrent.threadDelay $ fromIntegral $ Time.toMicroseconds $ Time.fromMilliseconds delayMs
          Task.Forever.kill process
          completedIterations <- Concurrent.readMVar mVar
          completedIterations `shouldSatisfy` (> 1)
      context "when an error is thrown during a Task iteration" $ do
        it "passes the correct value to onIterationResult" $ do
          mVar <- Concurrent.newMVar 0
          let onIterationResult _ result = case result of
                                             Left Error0 -> Task.Forever.terminate
                                             _        -> Task.Forever.loop
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $ do
            Task.liftIO $ Concurrent.modifyMVar_ mVar (return . (+ 1))
            Task.throwError Error0
          Task.Forever.wait process
          Concurrent.readMVar mVar `shouldReturn` 1
      context "when an exception is raised during the execution of the Task" $ do
        it "passes the correct value to onIterationResult" $ do
          mVar <- Concurrent.newMVar 0
          let onIterationResult _ result = case result of
                                             Left (Exception0 TestException) -> Task.Forever.terminate
                                             _        -> Task.Forever.loop
          let config = Task.Forever.setOnIterationResult onIterationResult foreverConfig
          process <- Task.Forever.fork config TaskConfig0 $ do
            Task.liftIO $ Concurrent.modifyMVar_ mVar (return . (+ 1))
            Exception.throw TestException
          Task.Forever.wait process
          Concurrent.readMVar mVar `shouldReturn` 1
