module System.Log.Simple.Text (
    defaultTimeFormat,
    textFmt, text, shortText, msgOnly
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Text.Format

import System.Log.Simple.Base

-- | Default time format
defaultTimeFormat :: String
defaultTimeFormat = "%d/%m/%y %T %z"

textFmt :: String -> String -> Converter Text
textFmt tmFmt msgFmt (Message tm l p msg) = format msgFmt ~~ args where
    args = [
        "time" ~% formatTime defaultTimeLocale tmFmt tm,
        "level" ~% toStr l,
        "scope" ~% T.intercalate (T.pack "/") p,
        "message" ~% msg]
    toStr Trace = "TRACE"
    toStr Debug = "DEBUG"
    toStr Info = "INFO"
    toStr Warning = "WARN"
    toStr Error = "ERROR"
    toStr Fatal = "FATAL"

-- | Text log converter with default time format
text :: Converter Text
text = textFmt defaultTimeFormat "{time}\t{level}\t{scope}> {message}"

shortText :: Converter Text
shortText = textFmt defaultTimeFormat "{level}: {message}"

msgOnly :: Converter Text
msgOnly = textFmt defaultTimeFormat "{message}"
