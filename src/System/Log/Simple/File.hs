module System.Log.Simple.File (
    file
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Log.Simple.Base
import System.FilePath
import System.Directory
import System.IO

file :: FilePath -> Consumer Text
file fileName = Consumer withFileLog where
    withFileLog f = do
        createDirectoryIfMissing True $ takeDirectory fileName
        f $ \txt -> withFile fileName AppendMode $ \h -> do
            hSetEncoding h utf8
            T.hPutStrLn h txt
