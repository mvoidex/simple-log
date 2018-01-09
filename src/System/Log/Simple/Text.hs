module System.Log.Simple.Text (
	defaultTimeFormat,
	textFmt, text, shortText, msgOnly
	) where

import Data.Time
import Text.Format

import System.Log.Simple.Base

-- | Default time format
defaultTimeFormat ∷ String
defaultTimeFormat = "%_Y-%m-%d %T.%3q %z"

textFmt ∷ FormatResult r ⇒ String → String → Converter r
textFmt tmFmt msgFmt (Message tm l comp scope msg) = format msgFmt ~~ args where
	args = [
		"time" ~% formatTime defaultTimeLocale tmFmt tm,
		"level" ~% l,
		"component" ~% comp,
		"scope" ~% scope,
		"message" ~% msg]

-- | Text log converter with default time format
text ∷ FormatResult r ⇒ Converter r
text = textFmt defaultTimeFormat "{time:cyan}\t{level}\t{component:magenta}:{scope:green}> {message}"

shortText ∷ FormatResult r ⇒ Converter r
shortText = textFmt defaultTimeFormat "{level}\t{component:magenta}: {message}"

msgOnly ∷ FormatResult r ⇒ Converter r
msgOnly = textFmt defaultTimeFormat "{message}"
