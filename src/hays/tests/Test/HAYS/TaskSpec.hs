{-# LANGUAGE OverloadedStrings #-}

module Test.HAYS.TaskSpec
    ( spec
    ) where

import qualified Control.Concurrent     as Concurrent
import           Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity as Identity
import           Data.Text              (Text)
import           HAYS.Logger            (Logger)
import qualified HAYS.Logger            as Logger
import           HAYS.Task              (TaskT)
import qualified HAYS.Task              as Task
import           Test.Hspec

logger :: Logger
logger = Task.defaultLogger

mockLogger :: Concurrent.MVar (Logger.Level, Text) -> Logger
mockLogger loggerMVar =
  Logger.fromIO $ curry $ Concurrent.putMVar loggerMVar

data Config
  = Config0
  | Config1 Config
  deriving (Eq, Show)

data Error = Error0 | Error1 deriving (Eq, Show)

type TaskIdentity a = TaskT Config Error Identity a

returnIdentity :: a -> TaskIdentity a
returnIdentity = return

runTaskIdentity :: TaskIdentity a -> Either Error a
runTaskIdentity = Identity.runIdentity . Task.runTaskT logger Config0

type TaskIO a = TaskT Config Error IO a

returnIO :: a -> TaskIO a
returnIO = return

runTaskIO :: TaskIO a -> IO (Either Error a)
runTaskIO = Task.runTaskT logger Config0

runTaskIOWithMockLogger :: TaskIO a -> IO (Concurrent.MVar (Logger.Level, Text), Either Error a)
runTaskIOWithMockLogger task = do
  loggerMVar <- Concurrent.newEmptyMVar
  result <- Task.runTaskT (mockLogger loggerMVar) Config0 task
  return (loggerMVar, result)

spec :: Spec
spec = do
  describe "TaskT" $ do
    describe "Instances" $ do
      describe "Functor" $ do
        describe "fmap" $ do
          it "correctly modifies a value" $ do
            let task = (+ 1) <$> returnIdentity 0
            runTaskIdentity task `shouldBe` Right 1
        describe "Laws" $ do
          it "satisfies the Identity Law" $ do
            let task = returnIdentity 0
            runTaskIdentity (fmap id task) `shouldBe` runTaskIdentity (id task)
          it "satisfies the Composition Law" $ do
            let f = (0 :)
            let g = (1 :)
            let task = returnIdentity []
            runTaskIdentity (fmap (f . g) task) `shouldBe` runTaskIdentity ((fmap f . fmap g) task)
      describe "Applicative" $ do
        describe "pure" $ do
          it "correctly creates a task" $ do
            runTaskIdentity (pure 0) `shouldBe` Right 0
        describe "<*>" $ do
          it "correctly performs sequential application" $ do
            let f = pure (+ 1)
            let task = pure 0
            runTaskIdentity (f <*> task) `shouldBe` Right 1
        describe "Laws" $ do
          it "satisfies the Identity Law" $ do
            let task = pure 0
            runTaskIdentity (pure id <*> task) `shouldBe` runTaskIdentity task
          it "satisfies the Composition Law" $ do
            let u = pure (0 :)
            let v = pure (1 :)
            let w = pure []
            runTaskIdentity (pure (.) <*> u <*> v <*> w) `shouldBe` runTaskIdentity (u <*> (v <*> w))
          it "satisfies the Homomorphism Law" $ do
            let f = (+ 1)
            let x = 0
            runTaskIdentity (pure f <*> pure x) `shouldBe` runTaskIdentity (pure (f x))
          it "satisfies the Interchange Law" $ do
            let u = pure (+ 1)
            let y = 0
            runTaskIdentity (u <*> pure y) `shouldBe` runTaskIdentity (pure ($ y) <*> u)
      describe "Monad" $ do
        describe "return" $ do
          it "correctly creates a task" $ do
            runTaskIdentity (return 0) `shouldBe` Right 0
        describe ">>=" $ do
          it "correctly composes two sequential actions" $ do
            let f = return . (+ 1)
            let task = pure 0
            runTaskIdentity (task >>= f) `shouldBe` Right 1
        describe "Laws" $ do
          it "satisfies the Left Identity Law" $ do
            let a = 0
            let k = return . (+ 1)
            runTaskIdentity (return a >>= k) `shouldBe` runTaskIdentity (k a)
          it "satisfies the Right Identity Law" $ do
            let m = return 0
            runTaskIdentity (m >>= return) `shouldBe` runTaskIdentity m
          it "satisfies the Associativity Law" $ do
            let m = return 0
            let k = return . (+ 1)
            let h = return . (+ 2)
            runTaskIdentity (m >>= (\x -> k x >>= h)) `shouldBe` runTaskIdentity ((m >>= k) >>= h)
        describe "relation to Applicative instance" $ do
          it "pure and return are compatible" $ do
            runTaskIdentity (pure 0) `shouldBe` runTaskIdentity (return 0)
          it "<*> and >>= are compatible" $ do
            let m1 = return (+ 1)
            let m2 = return 0
            runTaskIdentity (m1 <*> m2) `shouldBe` runTaskIdentity (m1 >>= (\x1 -> m2 >>= (return . x1)))
      describe "MonadError" $ do
        describe "throwError" $ do
          it "correctly throws an error" $ do
            runTaskIdentity (Task.throwError Error1 :: TaskIdentity ()) `shouldBe` Left Error1
        describe "catchError" $ do
          it "correctly catches an error" $ do
            runTaskIdentity (Task.catchError (Task.throwError Error1) return) `shouldBe` Right Error1
      describe "MonadIO" $ do
        describe "liftIO" $ do
          it "correctly lifts an IO action" $ do
            runTaskIO (Task.liftIO (return 1)) `shouldReturn` Right 1
      describe "MonadTrans" $ do
        describe "lift" $ do
          it "correctly lifts an underlying action" $ do
            runTaskIO (Task.lift (return 1)) `shouldReturn` Right 1
      describe "MonadReader" $ do
        describe "ask" $ do
          it "correctly gets the config" $ do
            runTaskIdentity Task.ask `shouldBe` Right Config0
        describe "asks" $ do
          it "correctly gets and modifies the config" $ do
            runTaskIdentity (Task.asks Just) `shouldBe` Right (Just Config0)
        describe "local" $ do
          it "correctly modifies the config for an underlying action" $ do
            runTaskIdentity (Task.local Config1 Task.ask) `shouldBe` Right (Config1 Config0)
    describe "Execution" $ do
      describe "runTaskT" $ do
        context "when no errors were thrown" $ do
          it "returns the correct value" $ do
            runTaskIdentity (return 0) `shouldBe` Right 0
        context "when an error was thrown" $ do
          it "returns the correct value" $ do
            runTaskIdentity (Task.throwError Error0 :: TaskIdentity ()) `shouldBe` Left Error0
          it "does not execute subsequent actions" $ do
            runTaskIO (Task.throwError Error0 >> Task.liftIO (fail "This exception should not be thrown." :: IO ())) `shouldReturn` Left Error0
    describe "Logging" $ do
      describe "info" $ do
        it "calls the correct underlying logger function" $ do
          (loggerMVar, _) <- runTaskIOWithMockLogger (Task.info "message")
          Concurrent.readMVar loggerMVar `shouldReturn` (Logger.Info, "message")
      describe "warn" $ do
        it "calls the correct underlying logger function" $ do
          (loggerMVar, _) <- runTaskIOWithMockLogger (Task.warn "message")
          Concurrent.readMVar loggerMVar `shouldReturn` (Logger.Warn, "message")
      describe "error'" $ do
        it "calls the correct underlying logger function" $ do
          (loggerMVar, _) <- runTaskIOWithMockLogger (Task.error' "message")
          Concurrent.readMVar loggerMVar `shouldReturn` (Logger.Error, "message")
      describe "debug'" $ do
        it "calls the correct underlying logger function" $ do
          (loggerMVar, _) <- runTaskIOWithMockLogger (Task.debug "message")
          Concurrent.readMVar loggerMVar `shouldReturn` (Logger.Debug, "message")
      describe "getLogger" $ do
        it "returns the correct logger" $ do
          loggerMVar <- Concurrent.newEmptyMVar
          Task.runTaskT (mockLogger loggerMVar) Config0 $ do
            logger <- Task.getLogger
            Task.liftIO $ Logger.info logger "message"
          Concurrent.readMVar loggerMVar `shouldReturn` (Logger.Info, "message")
      describe "localLogger" $ do
        it "correctly maps the logger for the child Task" $ do
          (loggerMVar, _) <- runTaskIOWithMockLogger $
            Task.localLogger (Logger.prefix (const "prefix ")) $
              Task.info "message"
          Concurrent.readMVar loggerMVar `shouldReturn` (Logger.Info, "prefix message")
