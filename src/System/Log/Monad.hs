{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module System.Log.Monad (
	withLog, log, scope_, scope, scoper
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

instance (MonadCatchIO m, MonadReader Log m) => MonadLog m where
	askLog = ask

withLog :: Log -> ReaderT Log m a -> m a
withLog l act = runReaderT act l

log :: (MonadLog m) => Level -> Text -> m ()
log l msg = do
	(Log ch) <- askLog
	tm <- liftIO getCurrentTime
	liftIO $ writeChan ch $ PostMessage (Message tm l msg)

scope_ :: (MonadLog m) => Text -> m a -> m a
scope_ s act = do
	(Log ch) <- askLog
	bracket_ (liftIO $ writeChan ch $ EnterScope s) (liftIO $ writeChan ch LeaveScope) act

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
