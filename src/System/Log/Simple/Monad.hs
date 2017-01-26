{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module System.Log.Simple.Monad (
    LogT(..),
    withNoLog, withLog,
    runLog, runNoLog,
    log, sendLog,
    scope_,
    scope,
    scopeM_,
    scopeM,
    scoper,
    scoperM,
    ignoreError,
    ignoreErrorM,
    trace,
    MonadLog(..)
    ) where

import Prelude hiding (log)

import Control.Exception (SomeException)
import Control.Concurrent.MSem
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Writer
import qualified Control.Monad.Writer.Strict as Strict
import Control.Monad.Except
import Control.Monad.Catch
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Log.Simple.Base

class (MonadIO m, MonadMask m) => MonadLog m where
    askLog :: m Log

instance MonadLog m => MonadLog (ReaderT r m) where
    askLog = lift askLog

instance MonadLog m => MonadLog (StateT s m) where
    askLog = lift askLog

instance MonadLog m => MonadLog (Strict.StateT s m) where
    askLog = lift askLog

instance (Monoid w, MonadLog m) => MonadLog (WriterT w m) where
    askLog = lift askLog

instance (Monoid w, MonadLog m) => MonadLog (Strict.WriterT w m) where
    askLog = lift askLog

newtype LogT m a = LogT { runLogT :: ReaderT Log m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Log, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans LogT where
    lift = LogT . lift

instance (MonadIO m, MonadMask m) => MonadLog (LogT m) where
    askLog = LogT ask

withNoLog :: LogT m a -> m a
withNoLog act = runReaderT (runLogT act) noLog

withLog :: Log -> LogT m a -> m a
withLog l act = runReaderT (runLogT act) l

runLog :: MonadIO m => RulesLoad -> [Logger] -> LogT m a -> m a
runLog rs ls act = do
    l <- liftIO $ newLog rs ls
    withLog l act

runNoLog :: LogT m a -> m a
runNoLog = withNoLog

log :: (MonadLog m) => Level -> Text -> m ()
log l msg = do
    (Log post _ _) <- askLog
    tm <- liftIO getZonedTime
    liftIO $ post $ PostMessage (Message tm l [] msg)

sendLog :: MonadLog m => Level -> Text -> m ()
sendLog = log

scope_ :: (MonadLog m) => Text -> m a -> m a
scope_ s act = do
    (Log post _ getRules) <- askLog
    rs <- liftIO getRules
    sem <- liftIO $ new (0 :: Integer)
    bracket_ (liftIO $ post $ EnterScope s rs) (liftIO (post (LeaveScope $ signal sem) >> wait sem)) act

-- | Scope with log all exceptions
scope :: (MonadLog m) => Text -> m a -> m a
scope s act = scope_ s $ catch act onError where
    onError :: (MonadLog m) => SomeException -> m a
    onError e = do
        log Error $ T.concat ["Scope leaves with exception: ", fromString . show $ e]
        throwM e

-- | Workaround: we must explicitely post 'LeaveScope'
scopeM_ :: (MonadLog m, MonadError e m) => Text -> m a -> m a
scopeM_ s act = do
    (Log post _ getRules) <- askLog
    rs <- liftIO getRules
    sem <- liftIO $ new (0 :: Integer)
    let
        close = liftIO $ do
            post $ LeaveScope $ signal sem
            wait sem
    bracket_ (liftIO $ post $ EnterScope s rs) close (catchError act (\e -> close >> throwError e))

-- | Scope with log exceptions from 'MonadError'
-- | Workaround: we must explicitely post 'LeaveScope'
scopeM :: (Show e, MonadLog m, MonadError e m) => Text -> m a -> m a
scopeM s act = scopeM_ s $ catch act' onError' where
    onError' :: (MonadLog m) => SomeException -> m a
    onError' e = logE e >> throwM e
    act' = catchError act onError
    onError :: (MonadLog m, Show e, MonadError e m) => e -> m a
    onError e = logE e >> throwError e
    logE :: (MonadLog m, Show e) => e -> m ()
    logE e = log Error $ T.concat ["Scope leaves with exception: ", fromString . show $ e]

-- | Scope with tracing result
scoper :: (Show a, MonadLog m) => Text -> m a -> m a
scoper s act = do
    r <- scope s act
    log Trace $ T.concat ["Scope ", s, " leaves with result: ", fromString . show $ r]
    return r

scoperM :: (Show e, Show a, MonadLog m, MonadError e m) => Text -> m a -> m a
scoperM s act = do
    r <- scopeM s act
    log Trace $ T.concat ["Scope", s, " leaves with result: ", fromString . show $ r]
    return r

-- | Ignore error
ignoreError :: (MonadLog m) => m () -> m ()
ignoreError act = catch act onError where
    onError :: (MonadLog m) => SomeException -> m ()
    onError _ = return ()

-- | Ignore MonadError error
ignoreErrorM :: (MonadLog m, MonadError e m) => m () -> m ()
ignoreErrorM act = catchError act onError where
    onError :: MonadLog m => e -> m ()
    onError _ = return ()

-- | Trace value
trace :: (Show a, MonadLog m) => Text -> m a -> m a
trace name act = do
    v <- act
    log Trace $ T.concat [name, " = ", fromString . show $ v]
    return v
