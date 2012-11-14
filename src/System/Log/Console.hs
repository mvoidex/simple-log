module System.Log.Console (
    console
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Log.Base
import System.IO

console :: Consumer Text
console = Consumer withConsole where
    withConsole f = do
        hSetEncoding stdout utf8
        f T.putStrLn
