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
    let
        putText txt = withFile f AppendMode $ \h -> T.hPutStrLn h txt
    return $ Consumer (not ex) putText (return ())
