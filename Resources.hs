module Resources where

import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Environment.FindBin (getProgPath)

resourcePath :: FilePath -> FilePath
resourcePath res_name = resourceDir </> res_name

resourceDir :: FilePath
resourceDir = res_path where
    -- Assuming the path to binary will not change and saving ourselves a lot of work
    bin_path = unsafePerformIO getProgPath
    res_path = dropFileName bin_path </> "Resources"
