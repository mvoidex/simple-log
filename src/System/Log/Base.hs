module System.Log.Base (
    Level(..),
    Message(..),
    Converter(..), Consumer(..),
    Entry(..), Command(..),
    entries, flatten, untrace,
    Logger(..), logger,
    Log(..),
    newLog,
    writeLog,
    scopeLog_,
    scopeLog,
    scoperLog
    ) where

import Prelude hiding (log)

import Control.Arrow
import qualified Control.Exception as E
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

-- | Level of message
data Level = Trace | Debug | Info | Warning | Error | Fatal
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Log message
data Message = Message {
    messageTime :: UTCTime,
    messageLevel :: Level,
    messagePath :: [Text],
    messageText :: Text }
        deriving (Eq, Ord, Read, Show)

-- | Converts message some representation
data Converter a = Converter {
    initial :: a,
    convert :: Message -> a }

-- Stores message
data Consumer a = Consumer {
    consumerNew :: Bool,
    consume :: a -> IO (),
    consumerClose :: IO () }

-- | Logger
data Logger = Logger {
    loggerLog :: Message -> IO (),
    loggerClose :: IO () }

-- | Convert consumer creater to logger creater
logger :: Converter a -> IO (Consumer a) -> IO Logger
logger converter consumer = do
    c <- consumer
    when (consumerNew c) $ consume c (initial converter)
    return $ Logger (consume c . convert converter) (consumerClose c)

-- | Log
data Log = Log {
    logChan :: Chan Command }

-- | Create log
newLog :: [IO Logger] -> IO Log
newLog [] = error "Specify at least one log consumer"
newLog ls = do
    ch <- newChan
    cts <- getChanContents ch
    let
        msgs = flatten . untrace . entries $ cts
        startLog l = do
            l' <- l
            forkIO $ E.finally
                (mapM_ (loggerLog l') msgs)
                (loggerClose l')
    mapM_ startLog ls
    return $ Log ch

-- | Write message to log
writeLog :: Log -> Level -> Text -> IO ()
writeLog (Log ch) l msg = do
    tm <- getCurrentTime
    writeChan ch $ PostMessage (Message tm l [] msg)

-- | New log-scope
scopeLog_ :: Log -> Text -> IO a -> IO a
scopeLog_ (Log ch) s act = E.bracket_ (writeChan ch $ EnterScope s) (writeChan ch LeaveScope) act

-- | New log-scope with lifting exceptions as errors
scopeLog :: Log -> Text -> IO a -> IO a
scopeLog l s act = scopeLog_ l s (E.catch act onError) where
    onError :: E.SomeException -> IO a
    onError e = do
        writeLog l Error $ T.pack $ "Scope leaves with exception: " ++ show e
        E.throwIO e

-- | New log-scope with tracing scope result
scoperLog :: Show a => Log -> Text -> IO a -> IO a
scoperLog l s act = do
    r <- scopeLog l s act
    writeLog l Trace $ T.concat [T.pack "Scope ", s, T.pack " leaves with result: ", T.pack $ show r]
    return r

-- | Log entry, scope or message
data Entry =
    Entry Message |
    Scope Text [Entry]
        deriving (Eq, Read, Show)

foldEntry :: (Message -> a) -> (Text -> [a] -> a) -> Entry -> a
foldEntry r s (Entry m) = r m
foldEntry r s (Scope t rs) = s t (map (foldEntry r s) rs)

-- | Command to logger
data Command =
    EnterScope Text |
    LeaveScope |
    PostMessage Message

-- | Apply commands to construct list of entries
entries :: [Command] -> [Entry]
entries = fst . entries' where
    entries' [] = ([], [])
    entries' (EnterScope s : cs) = first (Scope s rs :) $ entries' cs' where
        (rs, cs') = entries' cs
    entries' (LeaveScope : cs) = ([], cs)
    entries' (PostMessage m : cs) = first (Entry m :) $ entries' cs

-- | Flattern entries to raw list of messages
flatten :: [Entry] -> [Message]
flatten = concatMap $ foldEntry return (\s ms -> map (addScope s) (concat ms)) where
    addScope s (Message tm l p str) = Message tm l (p ++ [s]) str

-- | Remove unnecessary traces
untrace :: [Entry] -> [Entry]
untrace = map untraceScope . concatEntries . first (partition isNotTrace) . break isError where
    -- untrace inner scopes
    untraceScope = foldEntry Entry (\t rs -> Scope t (untrace rs))

    -- If there is no errors, use only infos and scopes and drop all traces
    -- otherwise concat all messages
    concatEntries ((x, y), z) = x ++ if null z then [] else y ++ z

    isError = onLevel False (> Info)
    isNotTrace = onLevel True (== Info)

    onLevel :: a -> (Level -> a) -> Entry -> a
    onLevel v f (Scope _ _) = v
    onLevel v f (Entry (Message _ l _ _)) = f l
