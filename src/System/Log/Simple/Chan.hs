module System.Log.Simple.Chan (
	chan
	) where

import Control.Concurrent.Chan (Chan, writeChan)
import System.Log.Simple.Base

chan ∷ Chan a → Consumer a
chan ch = Consumer withChan where
	withChan f = f (writeChan ch)
