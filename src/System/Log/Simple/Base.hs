{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module System.Log.Simple.Base (
	Level(..), level, level_,
	Component(..), Scope(..),
	Message(..),
	Converter, Consumer(..),
	LogHandler, handler,
	LogConfig(..), defCfg, logCfg, componentCfg, componentLevel,
	Log(..),
	newLog, rootLog, getLog, subLog, updateLogConfig, writeLog, stopLog,
	) where

import Prelude.Unicode

import Control.Applicative
import qualified Control.Exception as E
import Control.Concurrent
import qualified Control.Concurrent.Async as A
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.String
import Lens.Micro.Platform
import Text.Format


-- Helper function
splitBy ∷ Char → Text → [Text]
splitBy _ "" = []
splitBy ch t = T.split (≡ ch) t



-- | Level of message
data Level = Trace | Debug | Info | Warning | Error | Fatal
	deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Default Level where
	def = Trace

-- | Component — each one have separate log scopes and can have different politics
-- Child component's root politics inherits its parent root politics
-- Component name parts stored in reverse order
newtype Component = Component { componentPath ∷ [Text] } deriving (Eq, Ord)

instance Show Component where
	show = T.unpack ∘ T.intercalate "." ∘ reverse ∘ componentPath

instance FormatBuild Component

instance Read Component where
	readsPrec _ = return ∘ flip (,) "" ∘ Component ∘ reverse ∘ splitBy '.' ∘ T.pack

instance IsString Component where
	fromString = read

instance Monoid Component where
	mempty = Component []
	Component l `mappend` Component r = Component $ r ++ l

instance NFData Component where
	rnf (Component cs) = rnf cs

-- | Log scope, also stored in reverse order
newtype Scope = Scope { scopePath ∷ [Text] } deriving (Eq, Ord)

instance Show Scope where
	show = T.unpack ∘ T.intercalate "/" ∘ reverse ∘ scopePath

instance FormatBuild Scope

instance Read Scope where
	readsPrec _ = return ∘ flip (,) "" ∘ Scope ∘ reverse ∘ splitBy '/' ∘ T.pack

instance IsString Scope where
	fromString = read

instance Monoid Scope where
	mempty = Scope []
	Scope l `mappend` Scope r = Scope $ r ++ l

instance NFData Scope where
	rnf (Scope s) = rnf s

class HasParent a where
	getParent ∷ a → Maybe a

instance HasParent Component where
	getParent (Component []) = Nothing
	getParent (Component (_:cs)) = Just $ Component cs

instance HasParent Scope where
	getParent (Scope []) = Nothing
	getParent (Scope (_:ps)) = Just $ Scope ps

-- | Parse level
level ∷ Text → Maybe Level
level = flip M.lookup levels ∘ T.toLower where
	levels = M.fromList [(T.toLower ∘ T.pack ∘ show $ l', l') | l' ← [minBound .. maxBound]]

-- | Parse level, failing on invalid input
level_ ∷ Text → Level
level_ t = fromMaybe (error errMsg) ∘ level $ t where
	errMsg = "invalid level: " ++ T.unpack t

-- | Log message
data Message = Message {
	messageTime ∷ ZonedTime,
	messageLevel ∷ Level,
	messageComponent ∷ Component,
	messageScope ∷ Scope,
	messageText ∷ Text }
		deriving (Read, Show)

instance NFData Message where
	rnf (Message t l c s m) = t `seq` l  `seq` rnf c `seq` rnf s `seq` rnf m

-- | Converts message some representation
type Converter a = Message → a

-- Stores message
data Consumer a = Consumer {
	withConsumer ∷ ((a → IO ()) → IO ()) → IO () }

-- | Message handler
type LogHandler = Consumer Message

-- | Convert consumer creater to logger creater
handler ∷ Converter a → Consumer a → Consumer Message
handler conv (Consumer withCons) = Consumer withCons' where
	withCons' f = withCons $ \logMsg → f (logMsg . conv)

data LogConfig = LogConfig {
	_logConfigMap ∷ Map Component Level }

instance Default LogConfig where
	def = LogConfig mempty

instance Show LogConfig where
	show (LogConfig cfg) = unlines [show comp ++ ":" ++ show lev | (comp, lev) ← M.toList cfg]

-- | Default log config — info level
defCfg ∷ LogConfig
defCfg = def

-- | Make log config by list of components and levels
logCfg ∷ [(Component, Level)] → LogConfig
logCfg = LogConfig ∘ M.fromList

makeLenses ''LogConfig

-- | Component config level lens
componentCfg ∷ Component → Lens' LogConfig (Maybe Level)
componentCfg comp = logConfigMap . at comp

-- | Get politics for specified component
componentLevel ∷ LogConfig → Component → Level
componentLevel cfg comp = fromMaybe def $ (cfg ^. componentCfg comp) <|> (componentLevel cfg <$> getParent comp)

-- | Log
data Log = Log {
	-- | Current log component
	logComponent ∷ Component,
	-- | Current log scope
	logScope ∷ Scope,
	-- | Log message, it is low-level function, i.e. it doesn't take into account current component and scope and writes message as is
	logPost ∷ Message → IO (),
	-- | Stop log and wait until it writes all
	logStop ∷ IO (),
	-- | Log config
	logConfig ∷ MVar LogConfig }

type FChan a = Chan (Maybe a)
type LogId = (Component, ThreadId)
type LogJob = (A.Async (), FChan Message)
type LogMap = Map LogId LogJob

writeFChan ∷ FChan a → a → IO ()
writeFChan ch = writeChan ch ∘ Just

stopFChan ∷ FChan a → IO ()
stopFChan ch = writeChan ch Nothing

getFChanContents ∷ FChan a → IO [a]
getFChanContents = liftM (catMaybes ∘ takeWhile isJust) ∘ getChanContents

-- | Create log, returns root logger for root component
--
-- Messages from distinct threads and components are splitted in several chans, where they are processed, and then messages combined back and sent to log-thread
--
newLog ∷ LogConfig → [LogHandler] → IO Log
newLog cfg handlers = do
	ch ← newChan ∷ IO (FChan (LogId, Message))
	chOut ← newChan ∷ IO (FChan Message)
	cts ← getFChanContents ch
	msgs ← getFChanContents chOut
	cfgVar ← newMVar cfg

	let
		-- | Write commands from separate threads to separate channels
		process ∷ LogMap → (LogId, Message) → IO LogMap
		process m (logId, msg) = do
			(thAsync, thChan) ← maybe newChild return $ M.lookup logId m
			writeFChan thChan msg
			return $ M.insert logId (thAsync, thChan) m

		-- | Stop all spawned asyncs for threads
		stopChildren ∷ LogMap → IO ()
		stopChildren m = do
			forM_ (M.elems m) $ \(thAsync, thChan) → do
				stopFChan thChan
				A.wait thAsync
			stopFChan chOut

		-- | Pass message firther if it passes config
		passMessage ∷ (Message → IO ()) → Message → IO ()
		passMessage fn msg = do
			cfg' ← readMVar cfgVar
			when (componentLevel cfg' (messageComponent msg) ≤ messageLevel msg) $ fn msg

		-- | New chan for thread, accepts commands from thread, writes processed messages to log-thread
		newChild ∷ IO (A.Async (), FChan Message)
		newChild = do
			thChan ← newChan
			thCts ← getFChanContents thChan
			thAsync ← A.async $ mapM_ (passMessage $ writeFChan chOut) thCts
			return (thAsync, thChan)

		fatalMsg ∷ String → IO Message
		fatalMsg s = do
			tm ← getZonedTime
			return $ Message tm Fatal "*" "" $ fromString s

		-- | Perform log
		tryLog ∷ (Message → IO ()) → Message → IO ()
		tryLog logMsg m = E.handle onError (m `deepseq` logMsg m) where
			onError ∷ E.SomeException → IO ()
			onError e = E.handle ignoreError $ fatalMsg ("Exception during logging message: " ++ show e) >>= logMsg
			ignoreError ∷ E.SomeException → IO ()
			ignoreError _ = return ()

		-- | Initialize all loggers
		startLog ∷ LogHandler → IO ()
		startLog (Consumer withCons) = withCons $ \logMsg → do
			mapM_ (tryLog logMsg) msgs

		-- | Write message with myThreadId
		writeMessage ∷ Message → IO ()
		writeMessage msg = do
			my ← myThreadId
			writeFChan ch ((messageComponent msg, my), msg)

	p ← A.async $ void $ do
		m ← foldM process M.empty cts
		stopChildren m
	mapM_ (forkIO ∘ startLog) handlers
	return $ Log mempty mempty writeMessage (stopFChan ch >> A.wait p) cfgVar

-- | Get root log, i.e. just drop current component and scope
rootLog ∷ Log → Log
rootLog l = l { logComponent = mempty, logScope = mempty }

-- | Get log for specified component and scope
getLog ∷ Component → Scope → Log → Log
getLog comp scope l = l { logComponent = comp, logScope = scope }

-- | Get sub-log
subLog ∷ Component → Scope → Log → Log
subLog comp scope l = l {
	logComponent = logComponent l `mappend` comp,
	logScope = logScope l `mappend` scope }

-- | Modify log config
updateLogConfig ∷ MonadIO m ⇒ Log → (LogConfig → LogConfig) → m ()
updateLogConfig l update = liftIO $ modifyMVar_ (logConfig l) (return ∘ update)

-- | Write message to log for current component and scope
writeLog ∷ MonadIO m ⇒ Log → Level → Text → m ()
writeLog (Log comp scope post _ _) l msg = liftIO $ do
	tm ← getZonedTime
	post $ Message tm l comp scope msg

-- | Wait log messages and stop log
stopLog ∷ MonadIO m ⇒ Log → m ()
stopLog = liftIO ∘ logStop
