module System.Log.Console (
    console
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Log.Base
import System.IO

console :: IO (Consumer Text)
console = do
    hSetEncoding stdout utf8
    return $ Consumer True T.putStrLn (return ())
