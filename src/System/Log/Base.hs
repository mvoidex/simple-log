{-# LANGUAGE OverloadedStrings #-}

module System.Log.Base (
    Level(..),
    Politics(..), Rule(..), Rules,
    defaultPolitics, debugPolitics, tracePolitics, silentPolitics, supressPolitics,
    rule, absolute, relative, child, root, path,
    (%=),
    politics, use, low, high,
    Message(..),
    Converter, Consumer(..),
    Entry(..), Command(..),
    entries, flatten, rules,
    Logger, logger,
    RulesLoad,
    Log(..), noLog,
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
import Control.DeepSeq
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.String

-- | Level of message
data Level = Trace | Debug | Info | Warning | Error | Fatal
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Scope politics
data Politics = Politics {
    politicsLow :: Level,
    politicsHigh :: Level }
        deriving (Eq, Ord, Read, Show)

-- | Default politics
defaultPolitics :: Politics
defaultPolitics = Politics Info Warning

-- | Debug politics
debugPolitics :: Politics
debugPolitics = Politics Debug Info

-- | Trace politics
tracePolitics :: Politics
tracePolitics = Politics Trace Info

-- | Silent politics
silentPolitics :: Politics
silentPolitics = Politics Info Fatal

-- | Supress all messages politics
supressPolitics :: Politics
supressPolitics = Politics Fatal Fatal

-- | Rule for politics
data Rule = Rule {
    rulePath :: [Text] -> Bool,
    rulePolitics :: Politics -> Politics }

type Rules = [Rule]

-- | Make rule
rule :: ([Text] -> Bool) -> (Politics -> Politics) -> Rule
rule = Rule

-- | Absolute scope-path
absolute :: [Text] -> [Text] -> Bool
absolute p = (== p)

-- | Relative scope-path
relative :: [Text] -> [Text] -> Bool
relative p = (p `isSuffixOf`)

-- | Scope-path for child
child :: ([Text] -> Bool) -> [Text] -> Bool
child _ [] = False
child r (_:ps) = r ps

-- | Root scope-path
root :: [Text] -> Bool
root = null

-- | Scope-path by text
--
-- @
-- \/ -- root
-- foo\/bar -- relative
-- \/foo\/bar -- absolute
-- foo\/bar\/ -- child of relative
-- \/foo\/bar\/ -- child of absolute
-- @
--
path :: Text -> ([Text] -> Bool)
path "/" = root
path p = path' $ T.split (== '/') p where
    path' ps
        | null ps = const False
        | T.null (head ps) && T.null (last ps) = child . absolute . init . tail $ ps
        | T.null (head ps) = absolute . tail $ ps
        | T.null (last ps) = child . relative . init $ ps
        | otherwise = relative ps

-- | Rule by path
(%=) :: Text -> (Politics -> Politics) -> Rule
p %= r = rule (path p) r

-- | Just set new politics
politics :: Level -> Level -> Politics -> Politics
politics l h _ = Politics l h

-- | Use predefined politics
use :: Politics -> Politics -> Politics
use p _ = p

-- | Set new low level
low :: Level -> Politics -> Politics
low l (Politics _ h) = Politics l h

-- | Set new high level
high :: Level -> Politics -> Politics
high h (Politics l _) = Politics l h

-- | Log message
data Message = Message {
    messageTime :: ZonedTime,
    messageLevel :: Level,
    messagePath :: [Text],
    messageText :: Text }
        deriving (Read, Show)

instance NFData Message where
    rnf (Message t l p m) = t `seq` l `seq` rnf p `seq` rnf m

-- | Converts message some representation
type Converter a = Message -> a

-- Stores message
data Consumer a = Consumer {
    withConsumer :: ((a -> IO ()) -> IO ()) -> IO () }

-- | Logger
type Logger = Consumer Message

-- | Convert consumer creater to logger creater
logger :: Converter a -> Consumer a -> Consumer Message
logger conv (Consumer withCons) = Consumer withCons' where
    withCons' f = withCons $ \logMsg -> f (logMsg . conv)

-- | Log
data Log = Log {
    logPost :: Command -> IO (),
    logRules :: IO Rules }

-- | Empty log
noLog :: Log
noLog = Log (const (return ())) (return [])

-- | Type to initialize rule updater
type RulesLoad = IO (IO Rules)

-- | Create log
--
-- Messages from distinct threads are splitted in several chans, where they are processed, and then messages combined back and sent to log-thread
--
newLog :: RulesLoad -> [Logger] -> IO Log
newLog _ [] = return noLog
newLog rsInit ls = do
    ch <- newChan :: IO (Chan (ThreadId, Command))
    chOut <- newChan :: IO (Chan Message)
    cts <- getChanContents ch
    msgs <- getChanContents chOut
    rs <- rsInit
    r <- rs

    let
        -- | Write commands from separate threads to separate channels
        process :: M.Map ThreadId (Chan Command) -> (ThreadId, Command) -> IO (M.Map ThreadId (Chan Command))
        process m (thId, cmd) = do
            thChan <- maybe newThreadChan return $ M.lookup thId m
            writeChan thChan cmd
            return $ M.insert thId thChan m

        -- | New chan for thread, accepts commands from thread, writes processed messages to log-thread
        newThreadChan :: IO (Chan Command)
        newThreadChan = do
            thChan <- newChan
            thCts <- getChanContents thChan
            _ <- forkIO $ mapM_ (writeChan chOut) $ uncommand thCts
            return thChan

        -- | Convert commands to messages
        uncommand :: [Command] -> [Message]
        uncommand = flatten . rules r [] . entries

        -- | Perform log
        tryLog :: (Message -> IO ()) -> Message -> IO ()
        tryLog logMsg m = E.handle onError (m `deepseq` logMsg m) where
            onError :: E.SomeException -> IO ()
            onError e = E.handle ignoreError $ do
                tm <- getZonedTime
                logMsg $ Message tm Error ["*"] $ fromString $ "Exception during logging message: " ++ show e
            ignoreError :: E.SomeException -> IO ()
            ignoreError _ = return ()

        -- | Initialize all loggers
        startLog :: Logger -> IO ()
        startLog (Consumer withCons) = withCons $ \logMsg -> do
            mapM_ (tryLog logMsg) msgs

        -- | Write command with myThreadId
        writeCommand :: Command -> IO ()
        writeCommand cmd = do
            i <- myThreadId
            writeChan ch (i, cmd)

    void $ forkIO $ void $ foldM process M.empty cts
    mapM_ (forkIO . startLog) ls
    return $ Log writeCommand rs

-- | Write message to log
writeLog :: Log -> Level -> Text -> IO ()
writeLog (Log post _) l msg = do
    tm <- getZonedTime
    post $ PostMessage (Message tm l [] msg)

-- | New log-scope
scopeLog_ :: Log -> Text -> IO a -> IO a
scopeLog_ (Log post getRules) s act = do
    rs <- getRules
    E.bracket_ (post $ EnterScope s rs) (post LeaveScope) act

-- | New log-scope with lifting exceptions as errors
scopeLog :: Log -> Text -> IO a -> IO a
scopeLog l s act = scopeLog_ l s (E.catch act onError) where
    onError :: E.SomeException -> IO a
    onError e = do
        writeLog l Error $ fromString $ "Scope leaves with exception: " ++ show e
        E.throwIO e

-- | New log-scope with tracing scope result
scoperLog :: Show a => Log -> Text -> IO a -> IO a
scoperLog l s act = do
    r <- scopeLog l s act
    writeLog l Trace $ T.concat ["Scope ", s, " leaves with result: ", fromString . show $ r]
    return r

-- | Log entry, scope or message
data Entry =
    Entry Message |
    Scope Text Rules [Entry]

foldEntry :: (Message -> a) -> (Text -> Rules -> [a] -> a) -> Entry -> a
foldEntry r _ (Entry m) = r m
foldEntry r s (Scope t rs es) = s t rs (map (foldEntry r s) es)

-- | Command to logger
data Command =
    EnterScope Text Rules |
    LeaveScope |
    PostMessage Message

-- | Apply commands to construct list of entries
entries :: [Command] -> [Entry]
entries = fst . entries' where
    entries' [] = ([], [])
    entries' (EnterScope s scopeRules : cs) = first (Scope s scopeRules rs :) $ entries' cs' where
        (rs, cs') = entries' cs
    entries' (LeaveScope : cs) = ([], cs)
    entries' (PostMessage m : cs) = first (Entry m :) $ entries' cs

-- | Flattern entries to raw list of messages
flatten :: [Entry] -> [Message]
flatten = concatMap $ foldEntry return (\s _ ms -> map (addScope s) (concat ms)) where
    addScope s (Message tm l p str) = Message tm l (s : p) str

-- | Apply rules
rules :: Rules -> [Text] -> [Entry] -> [Entry]
rules rs rpath = map untraceScope . concatEntries . first (partition isNotTrace) . break isError where
    -- untrace inner scopes
    untraceScope (Entry msg) = Entry msg
    untraceScope (Scope t scopeRules es) = Scope t scopeRules $ rules scopeRules (t : rpath) es

    -- current politics
    ps = apply rs (reverse rpath) defaultPolitics

    -- If there is no errors, use only infos and scopes and drop all traces
    -- otherwise concat all messages
    concatEntries ((x, y), z) = x ++ if null z then [] else y ++ z

    isError = onLevel False (> politicsHigh ps)
    isNotTrace = onLevel True (>= politicsLow ps)
    
    onLevel :: a -> (Level -> a) -> Entry -> a
    onLevel v _ (Scope _ _ _) = v
    onLevel _ f (Entry (Message _ l _ _)) = f l

-- | Apply rules to path
apply :: Rules -> [Text] -> Politics -> Politics
apply rs = foldr (.) id . map applier . reverse . inits where
    applier :: [Text] -> Politics -> Politics
    applier spath = foldr (.) id . map rulePolitics . filter (`rulePath` spath) $ rs
