module System.Log.Simple.Stream (
	stream, console
	) where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Log.Simple.Base
import System.IO

stream ∷ Handle → Consumer Text
stream h = do
	liftIO $ hSetEncoding h utf8
	return $ \txt → T.hPutStrLn h txt >> hFlush h

console ∷ Consumer Text
console = stream stderr
