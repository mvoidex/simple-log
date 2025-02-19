{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes, TypeFamilies, CPP #-}

module System.Log.Simple.Base (
	Level(..), level, level_,
	Component(..), Scope(..),
	Message(..),
	Converter, Consumer, consumer,
	LogHandler, handler,
	LogConfig(..), defCfg, logCfg, componentCfg, componentLevel,
	Log(..),
	newLog, rootLog, getLog, subLog, getLogConfig, updateLogConfig, updateLogHandlers, writeLog, stopLog,
	) where

import Prelude.Unicode

import Control.Applicative
import Control.Arrow
import qualified Control.Exception as E
import Control.Concurrent
import qualified Control.Concurrent.Async as A
import Control.DeepSeq
import Control.Monad
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad.IO.Class (MonadIO, liftIO)
#endif
import Control.Monad.Cont
import Data.Default
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.String
import Lens.Micro.Platform
import Lens.Micro.Internal
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

instance Formattable Level where
	formattable Trace _ = "TRACE" `withFlags` ["gray"]
	formattable Debug _ = "DEBUG" `withFlags` ["yellow"]
	formattable Info _ = "INFO" `withFlags` ["blue"]
	formattable Warning _ = "WARN" `withFlags` ["darkyellow"]
	formattable Error _ = "ERROR" `withFlags` ["red"]
	formattable Fatal _ = "FATAL" `withFlags` ["bg=red"]

-- | Component — each one have separate log scopes and can have different politics
-- Child component's root politics inherits its parent root politics
-- Component name parts stored in reverse order
newtype Component = Component { componentPath ∷ [Text] } deriving (Eq, Ord)

instance Show Component where
	show = T.unpack ∘ T.intercalate "." ∘ reverse ∘ componentPath

instance Formattable Component

instance Read Component where
	readsPrec _ = return ∘ flip (,) "" ∘ Component ∘ reverse ∘ splitBy '.' ∘ T.pack

instance IsString Component where
	fromString = read

instance Semigroup Component where
	Component l <> Component r = Component $ r ++ l

instance Monoid Component where
	mempty = Component []
	Component l `mappend` Component r = Component $ r ++ l

instance NFData Component where
	rnf (Component cs) = rnf cs

-- | Log scope, also stored in reverse order
newtype Scope = Scope { scopePath ∷ [Text] } deriving (Eq, Ord)

instance Show Scope where
	show = T.unpack ∘ T.intercalate "/" ∘ reverse ∘ scopePath

instance Formattable Scope

instance Read Scope where
	readsPrec _ = return ∘ flip (,) "" ∘ Scope ∘ reverse ∘ splitBy '/' ∘ T.pack

instance IsString Scope where
	fromString = read

instance Semigroup Scope where
	Scope l <> Scope r = Scope $ r ++ l

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

type Converter a = Message → a

-- | Returns function which accepts consumed value
type Consumer a = ContT () IO (a → IO ())

-- | Make consumer
consumer ∷ (((a → IO ()) → IO ()) → IO ()) → Consumer a
consumer = ContT

-- | Message handler
type LogHandler = Consumer Message

handler ∷ Converter a → Consumer a → Consumer Message
handler conv = fmap (∘ conv)

data LogConfig = LogConfig {
	_logConfigMap ∷ Map Component Level }

makeLenses ''LogConfig

instance Default LogConfig where
	def = LogConfig mempty

instance Show LogConfig where
	show (LogConfig cfg) = unlines [show comp ++ ":" ++ show lev | (comp, lev) ← M.toList cfg]

type instance Index LogConfig = Component
type instance IxValue LogConfig = Level

instance Ixed LogConfig where
	ix n = logConfigMap ∘ ix n

instance At LogConfig where
	at n = logConfigMap ∘ at n

-- | Default log config — info level
defCfg ∷ LogConfig
defCfg = def

-- | Make log config by list of components and levels
logCfg ∷ [(Component, Level)] → LogConfig
logCfg = LogConfig ∘ M.fromList

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
	logConfig ∷ MVar LogConfig,
	-- | Handlers list
	logHandlers ∷ MVar [LogHandler],
	-- | Restart all handlers
	logRestartHandlers ∷ IO () }

type FChan a = Chan (Maybe a)

writeFChan ∷ FChan a → a → IO ()
writeFChan ch = writeChan ch ∘ Just

stopFChan ∷ FChan a → IO ()
stopFChan ch = writeChan ch Nothing

-- | Create log, returns root logger for root component
--
-- Messages from distinct threads and components are splitted in several chans, where they are processed, and then messages combined back and sent to log-thread
--
newLog ∷ LogConfig → [LogHandler] → IO Log
newLog cfg handlers = do
	ch ← newChan ∷ IO (FChan Message)
	cfgVar ← newMVar cfg
	handlersVar ← newMVar handlers
	handlersThread ← newEmptyMVar

	let
		-- | Pass message firther if it passes config
		passMessage ∷ (Message → IO ()) → Message → IO ()
		passMessage fn msg = do
			cfg' ← readMVar cfgVar
			when (componentLevel cfg' (messageComponent msg) ≤ messageLevel msg) $ fn msg

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

		-- | Consume messages and send to handlers
		runHandlers ∷ FChan Message → [LogHandler] → ContT () IO ()
		runHandlers inCh hs = do
			hs' ← sequence $ map (fmap tryLog) hs
			fix $ \loop' → do
				msg ← liftIO $ readChan inCh
				case msg of
					Just msg' → liftIO (mapM_ ($ msg') hs') >> loop'
					Nothing → return ()

		-- | Start handlers thread
		startHandlers ∷ IO (A.Async ())
		startHandlers = readMVar handlersVar >>= A.async ∘ flip runContT return ∘ runHandlers ch

		-- | Restart handlers thread
		restartHandlers ∷ IO ()
		restartHandlers = modifyMVar_ handlersThread $ \th → A.cancel th >> startHandlers

		-- | Write message with myThreadId
		writeMessage ∷ Message → IO ()
		writeMessage = passMessage (writeFChan ch)

		waitHandlers ∷ IO ()
		waitHandlers = readMVar handlersThread >>= A.wait

	startHandlers >>= putMVar handlersThread
	return $ Log {
		logComponent = mempty,
		logScope = mempty,
		logPost = writeMessage,
		logStop = stopFChan ch >> waitHandlers,
		logConfig = cfgVar,
		logHandlers = handlersVar,
		logRestartHandlers = restartHandlers }

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

-- | Read log config
getLogConfig ∷ MonadIO m ⇒ Log → m LogConfig
getLogConfig l = liftIO $ readMVar (logConfig l)

-- | Modify log config
updateLogConfig ∷ MonadIO m ⇒ Log → (LogConfig → LogConfig) → m LogConfig
updateLogConfig l update = liftIO $ modifyMVar (logConfig l) (return ∘ (update &&& id))

-- | Update log handlers, this restarts handlers thread
updateLogHandlers ∷ MonadIO m ⇒ Log → ([LogHandler] → [LogHandler]) → m ()
updateLogHandlers l update = liftIO $ modifyMVar_ (logHandlers l) (return ∘ update) >> logRestartHandlers l

-- | Write message to log for current component and scope
writeLog ∷ MonadIO m ⇒ Log → Level → Text → m ()
writeLog l lev msg = liftIO $ do
	tm ← getZonedTime
	logPost l $ Message tm lev (logComponent l) (logScope l) msg

-- | Wait log messages and stop log
stopLog ∷ MonadIO m ⇒ Log → m ()
stopLog = liftIO ∘ logStop
