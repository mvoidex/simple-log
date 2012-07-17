module System.Log.Console (
	console
	) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Log.Base

console :: IO (Consumer Text)
console = return $ Consumer True T.putStrLn (return ())
