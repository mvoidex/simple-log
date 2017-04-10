module System.Log.Simple.Text (
	defaultTimeFormat,
	textFmt, text, shortText, msgOnly
	) where

import Data.Text (Text)
import Data.Time
import Text.Format

import System.Log.Simple.Base

-- | Default time format
defaultTimeFormat ∷ String
defaultTimeFormat = "%_Y-%m-%d %T %z"

textFmt ∷ String → String → Converter Text
textFmt tmFmt msgFmt (Message tm l comp scope msg) = format msgFmt ~~ args where
	args = [
		"time" ~% formatTime defaultTimeLocale tmFmt tm,
		"level" ~% toStr l,
		"component" ~% comp,
		"scope" ~% scope,
		"message" ~% msg]
	toStr Trace = "TRACE"
	toStr Debug = "DEBUG"
	toStr Info = "INFO"
	toStr Warning = "WARN"
	toStr Error = "ERROR"
	toStr Fatal = "FATAL"

-- | Text log converter with default time format
text ∷ Converter Text
text = textFmt defaultTimeFormat "{time}\t{level}\t{component}:{scope}> {message}"

shortText ∷ Converter Text
shortText = textFmt defaultTimeFormat "{level}\t{component}: {message}"

msgOnly ∷ Converter Text
msgOnly = textFmt defaultTimeFormat "{message}"
