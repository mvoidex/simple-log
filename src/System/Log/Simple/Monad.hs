{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

module System.Log.Simple.Monad (
    withNoLog,
    withLog,
    log,
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
import Control.Monad.Error
import Control.Monad.CatchIO as C
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Log.Simple.Base

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
    sem <- liftIO $ new (0 :: Integer)
    bracket_ (liftIO $ post $ EnterScope s rs) (liftIO (post (LeaveScope $ signal sem) >> wait sem)) act

-- | Scope with log all exceptions
scope :: (MonadLog m) => Text -> m a -> m a
scope s act = scope_ s $ C.catch act onError where
    onError :: (MonadLog m) => SomeException -> m a
    onError e = do
        log Error $ T.concat ["Scope leaves with exception: ", fromString . show $ e]
        throw e

-- | Workaround: we must explicitely post 'LeaveScope'
scopeM_ :: (MonadLog m, MonadError e m) => Text -> m a -> m a
scopeM_ s act = do
    (Log post getRules) <- askLog
    rs <- liftIO getRules
    sem <- liftIO $ new (0 :: Integer)
    let
        close = liftIO $ do
            post $ LeaveScope $ signal sem
            wait sem
    bracket_ (liftIO $ post $ EnterScope s rs) close (catchError act (\e -> close >> throwError e))

-- | Scope with log exceptions from 'MonadError'
-- | Workaround: we must explicitely post 'LeaveScope'
scopeM :: (Error e, Show e, MonadLog m, MonadError e m) => Text -> m a -> m a
scopeM s act = scopeM_ s $ C.catch act' onError' where
    onError' :: (MonadLog m) => SomeException -> m a
    onError' e = logE e >> throw e
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

scoperM :: (Error e, Show e, Show a, MonadLog m, MonadError e m) => Text -> m a -> m a
scoperM s act = do
    r <- scopeM s act
    log Trace $ T.concat ["Scope", s, " leaves with result: ", fromString . show $ r]
    return r

-- | Ignore error
ignoreError :: (MonadLog m) => m () -> m ()
ignoreError act = C.catch act onError where
    onError :: (MonadLog m) => SomeException -> m ()
    onError _ = return ()

-- | Ignore MonadError error
ignoreErrorM :: (Error e, MonadLog m, MonadError e m) => m () -> m ()
ignoreErrorM act = catchError act onError where
    onError :: (Error e, MonadLog m, MonadError e m) => e -> m ()
    onError _ = return ()

-- | Trace value
trace :: (Show a, MonadLog m) => Text -> m a -> m a
trace name act = do
    v <- act
    log Trace $ T.concat [name, " = ", fromString . show $ v]
    return v
