-- | Fast start
--
-- Create log config:
--
-- @
--myCfg = logCfg [(\"\", Info), (\"app\", Trace), (\"app.sub\", Debug)]
-- @
--
-- Create log and run log monad
--
-- @
--run ∷ IO ()
--run = runLog myCfg [handler text (file \"out.log\")] $ yourFunction
-- @
--
-- Function within log monad:
--
-- @
--yourFunction ∷ MonadLog m ⇒ m ()
--yourFunction = component \"app\" $ scope \"your\" $ do
--	sendLog Trace \"Hello from your function\"
-- @
--
-- Each component can have different level in config, subcomponents are specified with '.'
-- Components have independent scopes
-- Scopes can be nested and separated with '/':
--
-- @
--function2 ∷ MonadLog m ⇒ m ()
--function2 = component \"app.sub\" $ scope \"foo\" $ do
--	scope \"bar/baz\" $ do
--		sendLog Info \"Component app.sub and scope foo/bar/baz\"
--		sendLog Trace \"Invisible: app.sub configured with debug level\"
--	sendLog Info \"Same component and scope foo\"
--	component \"module\" $ sendLog Info \"Component module and root scope\"
-- @
--
-- You can update config with @updateLogConfig@ function
-- And change handlers with @updateLogHandlers@
-- 
-- There're also global logger @globalLog@, that can be used with @runGlobalLog@
--
-- @
--test ∷ IO ()
--test = do
--	updateLogHandlers globalLog ([handler text (file \"test.log\")]:)
--	runGlobalLog $ sendLog Info \"This will go to test.log too\"
--	updateLogConfig globalLog (set (ix \"\") Debug)
--	runGlobalLog $ sendLog Debug \"Now debug logs too\"
-- @
--
module System.Log.Simple (
	module System.Log.Simple.Base,
	module System.Log.Simple.Monad,
	module System.Log.Simple.Text,
	module System.Log.Simple.Stream,
	module System.Log.Simple.File,

	globalLog,
	runGlobalLog, runConsoleLog, runLogMsgs, runLogTexts
	) where

import Prelude.Unicode

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask)
import Control.Concurrent
import Data.Maybe
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import System.Log.Simple.Base (
	Level(..), Message, Converter, Consumer, LogHandler, handler,
	LogConfig(..), defCfg, logCfg, componentCfg, Log(..),
	newLog, rootLog, getLog, subLog,
	updateLogConfig, updateLogHandlers, writeLog, stopLog)
import System.Log.Simple.Monad (
	MonadLog(..), LogT(..), noLog, withLog, runLog, sendLog,
	component, scope_, scope, scopeM, scoper, scoperM, trace,
	modifyLogConfig, modifyLogHandlers)
import System.Log.Simple.Text
import System.Log.Simple.Stream
import System.Log.Simple.File
import System.Log.Simple.Chan

globalLog ∷ Log
globalLog = unsafePerformIO $ newLog defCfg [handler text console]

runGlobalLog ∷ LogT IO a → IO a
runGlobalLog = withLog globalLog

runConsoleLog ∷ LogConfig → LogT IO a → IO a
runConsoleLog cfg = runLog cfg [handler text console]

runLogChan ∷ (MonadIO m, MonadMask m) ⇒ (Chan w → LogHandler) → LogConfig → LogT m a → m (a, [w])
runLogChan c cfg act = do
	ch ← liftIO newChan
	mch ← liftIO newChan
	_ ← liftIO $ forkIO $ getChanContents ch >>= mapM_ (writeChan mch . Just)
	r ← runLog cfg [c ch] act
	liftIO $ writeChan mch Nothing
	msgs ← liftIO ((catMaybes ∘ takeWhile isJust) <$> getChanContents mch)
	return (r, msgs)

runLogMsgs ∷ (MonadIO m, MonadMask m) ⇒ LogConfig → LogT m a → m (a, [Message])
runLogMsgs = runLogChan chan

runLogTexts ∷ (MonadIO m, MonadMask m) ⇒ Converter Text → LogConfig → LogT m a → m (a, [Text])
runLogTexts txt = runLogChan (handler txt ∘ chan)
