{-# LANGUAGE OverloadedStrings #-}

module System.Log.Config (
    parseRule, parseRules,
    parseRule_, parseRules_,
    constant, mvar, fileCfg
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import Control.Monad.Writer
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Log.Base

instance Error Text where
    noMsg = strMsg noMsg
    strMsg = T.pack

-- | Parse rule
--
-- Format:
--
-- @
--path: rule1, rule2
-- @
--
-- where \"path\" is argument for 'path', and \"rule\" is one of
--
--     * /low low-value/ for 'low'
--
--     * /high high-value/ for 'high'
--
--     * /set low-value high-value/ for 'politics'
--
--     * /use predefind/ for 'use'
--
-- Examples:
--
-- @
-- \/: use trace
-- \/foo: low trace
-- foo\/bar\/quux: use silent
-- @
--
parseRule :: Text -> Writer [Text] Rule
parseRule txt = do
    r' <- parseUses . T.strip . T.drop 1 $ r
    return $ T.strip p %= r'
    where
        (p, r) = T.break (== ':') txt
        parseUses uses = do
            tell fails
            return $ foldr (.) id oks
            where
                (fails, oks) = (lefts &&& rights) . map (parseUse . T.strip) . T.split (== ',') $ uses

        parseUse u = case map T.strip . T.words $ u of
            ["low", v] -> low <$> value v
            ["high", v] -> high <$> value v
            ["set", l, h] -> politics <$> value l <*> value h
            ["use", v] -> use <$> predefined v
            _ -> throwError $ T.concat ["Unable to parse: ", u]

        value v = maybe noValue return $ lookup v values where
            noValue = throwError $ T.concat ["Invalid value: ", v]

        predefined v = maybe noPredefined return $ lookup v predefineds where
            noPredefined = throwError $ T.concat ["Invalid predefined: ", v]

parseRules :: Text -> Writer [Text] Rules
parseRules = mapM parseRule . filter (not . T.null . T.strip) . T.lines

-- | Try parse rule ignoring errors
parseRule_ :: Text -> Rule
parseRule_ = fst . runWriter . parseRule

-- | Try parse rules ignoring errors
parseRules_ :: Text -> Rules
parseRules_ = fst . runWriter . parseRules

-- | Value names
values :: [(Text, Level)]
values = [
    ("trace", Trace),
    ("debug", Debug),
    ("info", Info),
    ("warning", Warning),
    ("error", Error),
    ("fatal", Fatal)]

-- | Predefined politics
predefineds :: [(Text, Politics)]
predefineds = [
    ("default", defaultPolitics),
    ("debug", debugPolitics),
    ("trace", tracePolitics),
    ("silent", silentPolitics)]

-- | Constant rules
constant :: Rules -> IO (IO Rules)
constant = return . return

-- | Rules from mvar
mvar :: MVar Rules -> IO (IO Rules)
mvar = return . readMVar

-- | Rules from file
fileCfg :: FilePath -> Int -> IO (IO Rules)
fileCfg f seconds = do
    rs <- readRules
    var <- newMVar rs
    when (seconds /= 0) $ void $ forkIO $ forever $ handle ignoreIO $ do
        threadDelay (seconds * 1000000)
        rs' <- readRules
        void $ swapMVar var rs'
    mvar var
    where
        readRules = do
            cts <- T.readFile f
            return . parseRules_ $ cts
        ignoreIO :: IOException -> IO ()
        ignoreIO _ = return ()
