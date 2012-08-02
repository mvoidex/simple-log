module System.Log.File (
    file
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Log.Base
import System.Directory
import System.IO

file :: FilePath -> IO (Consumer Text)
file f = do
    ex <- doesFileExist f
    h <- openFile f AppendMode
    let
        putText txt = do
            T.hPutStrLn h txt
            hFlush h
    return $ Consumer (not ex) putText (hFlush h >> hClose h)
