module System.Log.Text (
	defaultTimeFormat,
	textFmt, text
	) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Locale
import System.Log.Base

-- | Default time format
defaultTimeFormat = "%x %T"

-- | Text log converter with time format
textFmt :: String -> Converter Text
textFmt fmt = Converter T.empty c where
	c (Message tm l msg) = T.intercalate (T.pack "\t") [T.pack s, T.pack (toStr l), msg] where
		s = formatTime defaultTimeLocale fmt tm
	toStr Trace = "TRACE"
	toStr Debug = "DEBUG"
	toStr Info = "INFO"
	toStr Warning = "WARN"
	toStr Error = "ERROR"
	toStr Fatal = "FATAL"

-- | Text log converter with default time format
text :: Converter Text
text = textFmt defaultTimeFormat
