module System.Log.Simple.File (
	file
	) where

import Control.Monad.Cont
import Data.Text (Text)
import System.Log.Simple.Base
import System.FilePath
import System.Directory
import System.IO

import System.Log.Simple.Stream (stream)

file ∷ FilePath → Consumer Text
file fileName = do
	liftIO $ createDirectoryIfMissing True $ takeDirectory fileName
	h ← ContT $ withFile fileName AppendMode
	stream h
