{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module System.Log.Monad (
    withNoLog, withLog, log, scope_, scope, scoper, MonadLog(..)
    ) where

import Prelude hiding (log, catch)

import Control.Concurrent.Chan
import Control.Exception (SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.CatchIO
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Log.Base

class (MonadCatchIO m) => MonadLog m where
    askLog :: m Log

instance (MonadCatchIO m) => MonadLog (ReaderT Log m) where
    askLog = ask

withNoLog :: ReaderT Log m a -> m a
withNoLog act = runReaderT act noLog

withLog :: Log -> ReaderT Log m a -> m a
withLog l act = runReaderT act l

log :: (MonadLog m) => Level -> Text -> m ()
log l msg = do
    (Log post _) <- askLog
    tm <- liftIO getZonedTime
    liftIO $ post $ PostMessage (Message tm l [] msg)

scope_ :: (MonadLog m) => Text -> m a -> m a
scope_ s act = do
    (Log post getRules) <- askLog
    rs <- liftIO getRules
    bracket_ (liftIO $ post $ EnterScope s rs) (liftIO $ post LeaveScope) act

-- | Scope with log all exceptions
scope :: (MonadLog m) => Text -> m a -> m a
scope s act = scope_ s (catch act onError) where
    onError :: (MonadLog m) => SomeException -> m a
    onError e = do
        log Error $ T.pack $ "Scope leaves with exception: " ++ show e
        throw e

-- | Scope with tracing result
scoper :: (MonadLog m, Show a) => Text -> m a -> m a
scoper s act = do
    r <- scope s act
    log Trace $ T.concat [T.pack "Scope ", s, T.pack " leaves with result: ", T.pack $ show r]
    return r
