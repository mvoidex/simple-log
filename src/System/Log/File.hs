module System.Log.File (
    file
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Log.Base
import System.FilePath
import System.Directory
import System.IO

file :: FilePath -> IO (Consumer Text)
file f = do
    createDirectoryIfMissing True $ takeDirectory f
    ex <- doesFileExist f
    let
        putText txt = withFile f AppendMode $ \h -> do
            hSetEncoding h utf8
            T.hPutStrLn h txt
    return $ Consumer (not ex) putText (return ())
