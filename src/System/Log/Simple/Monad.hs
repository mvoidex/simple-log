{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts, CPP, ImplicitParams #-}

module System.Log.Simple.Monad (
	-- | Monad log
	MonadLog(..), LogT(..),
	noLog, withLog, runLog,

	-- | Getters
	askComponent, askScope,

	-- | Log functions
	log, sendLog,
	component,
	scope_, scope, scopeM, scoper, scoperM,
	trace,
	modifyLogConfig, modifyLogHandlers,
	) where

import Prelude hiding (log)
import Prelude.Unicode

#if __GLASGOW_HASKELL__ >= 800
import Control.Exception (SomeException)
#endif
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Reader
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import System.Log.Simple.Base

class (MonadIO m, MonadMask m) => MonadLog m where
	askLog ∷ m Log
	localLog ∷ (Log → Log) → m a → m a

instance {-# OVERLAPPABLE #-} (MonadLog m, MonadTrans t, MFunctor t, MonadIO (t m), MonadMask (t m)) ⇒ MonadLog (t m) where
	askLog = lift askLog
	localLog fn = hoist (localLog fn)

newtype LogT m a = LogT { runLogT ∷ ReaderT Log m a }
	deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader Log, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans LogT where
	lift = LogT ∘ lift

instance (MonadIO m, MonadMask m) => MonadLog (LogT m) where
	askLog = LogT ask
	localLog fn = LogT ∘ local fn ∘ runLogT

-- | Run with no logging
noLog ∷ (MonadIO m, MonadMask m) ⇒ LogT m a → m a
noLog = runLog defCfg []

-- | Run @LogT@ monad with @Log@
withLog ∷ Log → LogT m a → m a
withLog l act = runReaderT (runLogT act) l

-- | Run @LogT@ monad with log config and handlers
runLog ∷ (MonadIO m, MonadMask m) ⇒ LogConfig → [LogHandler] → LogT m a → m a
runLog cfg handlers = bracket (liftIO $ newLog cfg handlers) (liftIO ∘ stopLog) ∘ flip withLog

-- | Ask current component
askComponent ∷ MonadLog m ⇒ m Component
askComponent = logComponent <$> askLog

-- | Ask current scope
askScope ∷ MonadLog m ⇒ m Scope
askScope = logScope <$> askLog

-- | Log message
log ∷ MonadLog m ⇒ Level → Text → m ()
log lev msg = do
	l ← askLog
	writeLog l lev msg

-- | Log message, same as @log@
sendLog ∷ MonadLog m ⇒ Level → Text → m ()
sendLog = log

-- | Log component, also sets root scope
component ∷ MonadLog m ⇒ Text → m a → m a
component c = localLog (getLog (read ∘ T.unpack $ c) mempty)

-- | Create local scope
scope_ ∷ MonadLog m ⇒ Text → m a → m a
scope_ s = localLog (subLog mempty (read ∘ T.unpack $ s))

#if __GLASGOW_HASKELL__ < 800
type HasCallStack = ?callStack ∷ CallStack

callStack ∷ HasCallStack ⇒ CallStack
callStack = ?callStack

prettyCallStack ∷ CallStack → String
prettyCallStack = showCallStack
#endif

-- | Scope with log all exceptions
scope ∷ (MonadLog m, HasCallStack) ⇒ Text → m a → m a
scope s act = scope_ s $ catch act onErr where
	onErr ∷ MonadLog m ⇒ SomeException → m a
	onErr e = do
		log Error $ T.unlines [
			T.concat ["Scope leaves with exception: ", fromString ∘ show $ e],
			fromString $ prettyCallStack callStack]
		throwM e

-- | Scope with log exception from @MonadError@
scopeM ∷ (MonadLog m, MonadError e m, Show e, HasCallStack) ⇒ Text → m a → m a
scopeM s act = scope_ s $ catchError act onErr where
	onErr ∷ (MonadLog m, MonadError e m, Show e) ⇒ e → m a
	onErr e = do
		log Error $ T.unlines [
			T.concat ["Scope leaves with exception: ", fromString ∘ show $ e],
			fromString $ prettyCallStack callStack]
		throwError e

-- | Scope with tracing result
scoper ∷ (MonadLog m, Show a, HasCallStack) ⇒ Text → m a → m a
scoper s act = do
	r ← scope s act
	log Trace $ T.concat ["Scope ", s, " leaves with result: ", fromString . show $ r]
	return r

scoperM ∷ (MonadLog m, MonadError e m, Show e, Show a, HasCallStack) ⇒ Text → m a → m a
scoperM s act = do
	r ← scopeM s act
	log Trace $ T.concat ["Scope", s, " leaves with result: ", fromString . show $ r]
	return r

-- | Trace value
trace ∷ (MonadLog m, Show a) ⇒ Text → m a → m a
trace name act = do
	v ← act
	log Trace $ T.concat [name, " = ", fromString . show $ v]
	return v

-- | Modify config, same as @updateLogConfig@, but within @MonadLog@
modifyLogConfig ∷ MonadLog m ⇒ (LogConfig → LogConfig) → m LogConfig
modifyLogConfig fn = askLog >>= flip updateLogConfig fn

-- | Modify handlers, same as @updateLogHandlers@, but within @MonadLog@
modifyLogHandlers ∷ MonadLog m ⇒ ([LogHandler] → [LogHandler]) → m ()
modifyLogHandlers fn = askLog >>= flip updateLogHandlers fn
