-- | Fast start
--
-- The best way is to define config file, which is auto reloaded periodically, so you can change config while program is running to turn on tracing some function.
--
-- Typical config file with rule for root scope (see below for explanation):
--
-- @
-- \/: use default
-- @
--
-- If you want to trace scope named \"foo\", just add:
--
-- @
-- \/: use default
-- foo: low trace
-- @
--
-- Now \"foo\" and children will be traced even there are no errors. To trace only \"foo\" without children:
--
-- @
-- \/: use default
-- foo: low trace
-- foo\/: use default
-- @
--
-- \"foo\/\" defines rules for children of \"foo\".
--
-- Note, that by default all function will log their traces on error, so there is no need to turn on trace manually. You may want to turn on tracing when there are logic errors present without explicit errors (exceptions, or messages with error level).
--
-- Now we can run our log with auto reloading config every 60 seconds:
--
-- @
--run ∷ IO ()
--run = do
--	l <- newLog (fileCfg \"log.cfg\" 60) [logger text (file \"out.log\")]
--	withLog l yourFunction
-- @
--
-- And use it:
--
-- @
--yourFunction ∷ (MonadLog m) => m ()
--yourFunction = scope \"your\" $ do
--	log Trace \"Hello from your function\"
-- @
--
-- The main ideas of this log library are:
--
--	 * we don't want to see all unnecessary trace messages when there are no errors,
--
--	 * but we want to have all possible information about error.
-- 
-- This library is based on scopes. Every scope have a name, and logs traces only if there are some errors. Otherwise it logs only message with 'Info' level.
-- 
-- Let's start by simple example:
--
-- @
--test ∷ ReaderT Log IO ()
--test = scope \"test\" $ do
--log Trace \"Trace message\"
--	log Info \"Starting test\"
--	s \<- liftIO T.getLine
--	when (T.null s) $ log Error \"Oh no!\"
--	log Trace $ T.concat [\"Your input: \", s]
-- @
--
-- When you input some valid string, it will produce output:
--
-- @
--08\/10\/12 22:23:34   INFO	test> Starting test
--abc
-- @
--
-- wihtout any traces
--
-- But if you input empty strings, you'll get:
--
-- @
--08\/10\/12 22:24:20   INFO	test> Starting test
--08\/10\/12 22:24:20   TRACE   test> Trace message
--08\/10\/12 22:24:21   ERROR   test> Oh no!
--08\/10\/12 22:24:21   TRACE   test> Your input: 
-- @
--
-- Note, that first 'Trace' is written after 'Info', that's because logger don't know whether 'Trace' message will be written or not, but he must write 'Info' message immediately. But that's not a big problem.
--
-- There are three scope functions: 'scope_', 'scope' and 'scoper'. 'scope_' is basic function. 'scope' catches all exceptions and logs error with it, then rethrows. 'scoper' is like 'scope', but logs (with 'Trace' level) result of do-block.
--
-- Of course, scopes can be nested:
--
-- @
--test ∷ ReaderT Log IO ()
--test = scope \"test\" $ do
--	log Trace \"test trace\"
--	foo
--	log Info \"some info\"
--	bar
--
--foo ∷ ReaderT Log IO ()
--foo = scope \"foo\" $ do
--	log Trace \"foo trace\"
--
--bar ∷ ReaderT Log IO ()
--bar = scope \"bar\" $ do
--	log Trace \"bar trace\"
--	log Error \"bar error\"
-- @
--
-- Output:
--
-- @
--08\/10\/12 22:32:53   INFO	test> some info
--08\/10\/12 22:32:53   TRACE   test/bar> bar trace
--08\/10\/12 22:32:53   ERROR   test/bar> bar error
-- @
--
-- Note, no messages for \"foo\" and no trace messages for \"test\", because error was in \"bar\", not in \"foo\".
--
-- Code to run log:
--
-- @
--rules ∷ Rules
--rules = []
--
--run ∷ IO ()
--run = do
--	l <- newLog (constant rules) [logger text console]
--	withLog l test
-- @
--
-- Politics sets 'low' and 'high' levels. By default, 'low' and 'high' are INFO and WARN. Levels below 'low' are "traces" ('Trace' and 'Debug' by default). Levels above 'high' are "errors" ('Error' and 'Fatal' by default).
--
-- If you set 'low' to 'Trace', all messages will be written. If you set 'low' to 'Debug' and 'high' to 'Fatal', "traces" (in this case only 'Trace') will be never written.
--
-- Sometimes we need to trace function, but we don't want to write all traces. We can get this by setting rules. Rules changes politics for specified scope-path (scope-path is list of nested scopes, for example [\"test\"], [\"test\", \"bar\"], [\"test\", \"bar\", \"baz\", \"quux\"] etc.)
--
-- For example, we want to trace function 'foo':
--
-- @
--rules = [
--	rule root $ use defaultPolitics,
--	rule (relative [\"foo\"]) $ low Trace]
-- @
--
-- From now all scope-paths, that contains \"foo\" (all scopes with name \"foo\") will have politics with 'low' set to Trace.
--
-- We may adjust politics for scope 'foo', that is nested directly in scope 'quux':
--
-- @
--rules = [
--	rule root $ use defaultPolitics,
--	relative [\"quux\", \"foo\"] $ low Trace]
-- @
--
-- And, of course, we may specify absolute path:
--
-- @
--rules = [
--	rule root $ use defaultPolitics,
--	absolute [\"bar\", \"baz\", \"foo\"] $ low Trace]
-- @
-- 
-- Politics will be changed only for scope \"foo\", which is nested directly in \"baz\", which is nested in \"bar\", which is top scope.
--
-- Another way to define rule is using special functions from "System.Log.Config" module:
--
-- @
--rules = [
--	\"\/\" %= use defaultPolitics,
--	\"\/bar\/baz\/foo\" %= low Trace,
--	\"quux\/foo\" %= low Debug]
-- @
--
-- One more way to use special syntax for rules:
--
-- @
--rules = parseRules_ $ T.unlines [
--	\"\/: use default\",
--	\"\/bar\/baz\/foo: low trace\",
--	\"quux\/foo: low debug\"]
-- @
--
-- Here \"\/\" is for root, \"\/path\" for absolute path, \"path\" for relative and \"path\/\" for child of \"path\" (which may be also prefixed with \"\/\" to be absolute)
--
-- This syntax is useful to config log by file. Having file \"log.cfg\":
--
-- @
-- \/: use default
-- \/bar\/baz\/foo: low trace
-- quux\/foo: low debug
-- @
--
-- We can use it to config log
--
-- @
--	l <- newLog (fileCfg \"log.cfg\" 60) [logger text console]
-- @
-- 
-- where 60 is period (in seconds) of auto reload or 0 for no reloading.
--
module System.Log.Simple (
	module System.Log.Simple.Base,
	module System.Log.Simple.Config,
	module System.Log.Simple.Monad,
	module System.Log.Simple.Text,
	module System.Log.Simple.Console,
	module System.Log.Simple.File,

	globalLogger,
	runGlobalLog, runConsoleLog, runLogMsgs, runLogTexts
	) where

import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import System.Log.Simple.Base hiding (entries, flatten, rules)
import System.Log.Simple.Config
import System.Log.Simple.Monad
import System.Log.Simple.Text
import System.Log.Simple.Console
import System.Log.Simple.File
import System.Log.Simple.Chan

globalLogger ∷ Log
globalLogger = unsafePerformIO $ do
	cfg <- newMVar $ rules_ []
	newLog (mvarCfg cfg) [logger text console]

runGlobalLog ∷ LogT IO a → IO a
runGlobalLog = withLog globalLogger

runConsoleLog ∷ RulesLoad → LogT IO a → IO a
runConsoleLog rs = runLog rs [logger text console]

runLogChan ∷ MonadIO m => (Chan w → Logger) → RulesLoad → LogT m a → m (a, [w])
runLogChan c rs act = do
	ch <- liftIO newChan
	mch <- liftIO newChan
	_ <- liftIO $ forkIO $ getChanContents ch >>= mapM_ (writeChan mch . Just)
	r <- runLog rs [c ch] act
	liftIO $ writeChan mch Nothing
	msgs <- liftIO ((catMaybes . takeWhile isJust) <$> getChanContents mch)
	return (r, msgs)

runLogMsgs ∷ MonadIO m => RulesLoad → LogT m a → m (a, [Message])
runLogMsgs = runLogChan chan

runLogTexts ∷ MonadIO m => RulesLoad → LogT m a → m (a, [Text])
runLogTexts = runLogChan (logger text . chan)
