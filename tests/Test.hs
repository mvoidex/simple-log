{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Data.Text (Text)
import Test.Hspec

import System.Log.Simple

main ∷ IO ()
main = hspec $ do
	it "works" $ do
		(_, msgs) ← runLogTexts testText testCfg $ do
			component "main" $ do
				sendLog Trace "Should be invisible"
				sendLog Info "Should be visible"
			component "app" $ scope "foo" $ do
				sendLog Trace "This is visible"
				sendLog Debug "And this is visible too"
				component "app.sub" $ scope "bar/baz" $ scope "quux" $ do
					sendLog Debug "Debugs here are visible"
					sendLog Trace "But not traces"
			sendLog Trace "Root doesn't log traces"
			sendLog Info "Root logs infos"
		msgs `shouldBe` validMsgs

testCfg ∷ LogConfig
testCfg = logCfg [
	("", Info),
	("app", Trace),
	("app.sub", Debug)]

testText ∷ Converter Text
testText = textFmt defaultTimeFormat "{level} {component}:{scope}> {message}"

validMsgs ∷ [Text]
validMsgs = [
	"INFO main:> Should be visible",
	"TRACE app:foo> This is visible",
	"DEBUG app:foo> And this is visible too",
	"DEBUG app.sub:bar/baz/quux> Debugs here are visible",
	"INFO :> Root logs infos"]
