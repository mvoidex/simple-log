module System.Log.Simple.Stream (
	stream, console,
	coloredStream, coloredConsole
	) where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Log.Simple.Base
import System.IO
import Text.Format (Formatted)
import Text.Format.Colored

stream ∷ Handle → Consumer Text
stream h = do
	liftIO $ hSetEncoding h utf8
	return $ \txt → T.hPutStrLn h txt >> hFlush h

console ∷ Consumer Text
console = stream stderr

coloredStream ∷ Handle → Consumer Formatted
coloredStream h = do
	liftIO $ hSetEncoding h utf8
	return $ \f → hColored h f >> hFlush h

coloredConsole ∷ Consumer Formatted
coloredConsole = coloredStream stderr
