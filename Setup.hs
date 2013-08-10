import Distribution.MacOSX
import Distribution.Simple

import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths (autogenModulesDir, autogenModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Setup (BuildFlags(..), fromFlag)

import System.FilePath ((</>), (<.>))
import Control.Monad (when, liftM)
import Distribution.Simple.Utils (matchFileGlob, rewriteFile, createDirectoryIfMissingVerbose)
import Data.Maybe (fromJust)


applist :: [FilePath] -> [MacApp]
applist resources = [
    MacApp
        "SDLTemplate"
        (Just "SDLTemplate.icns")
        (Just "Info.plist") -- Build a default Info.plist for the icon.
        -- list or resources (to be copied to .app/Contents/Resources)
        resources
        [] -- No other binaries.
        -- collect non-standard dynamic libraries (to be copied to .app/Contents/Frameworks)
        ChaseWithDefaults
    ]


bundleAll :: LocalBuildInfo -> Bool
bundleAll lbi = fromJust $ lookup (FlagName "bundleall") (configConfigurationsFlags $ configFlags lbi)


bundleHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
bundleHook args bf pd lbi = do
    when (dataDir pd /= "")
        (ioError (userError "Non-empty data-dir is not supported"))

    -- Resolve all the globs in Data-files
    -- (appBundleBuildHook requires explicit file list)
    resources <- ((liftM concat) . (mapM matchFileGlob)) (dataFiles pd)
    let resources_to_bundle = if (bundleAll lbi) then resources else []

    appBundleBuildHook (applist resources_to_bundle) args bf pd lbi


pathsHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
pathsHook pd lbi uh bf = do
    -- creating our own paths module
    let dynpaths_module = "Dyn" ++ ModuleName.toFilePath (autogenModuleName pd)
    let contents = unlines [
            "module " ++ dynpaths_module ++ " where",
            "import System.FilePath",
            "import System.IO.Unsafe (unsafePerformIO)",
            "-- this functionality will be in System.Environment from ghc7.6",
            "import System.Environment.FindBin (getProgPath)",

            "resourceDir :: FilePath",
            "resourceDir = res_path where",
            "    -- Assuming the path to binary will not change and saving ourselves a lot of work",
            "    bin_path = unsafePerformIO getProgPath",
            (if (bundleAll lbi)
                then "    res_path = dropFileName bin_path </> \"Resources\""
                else "    res_path = joinPath $ reverse . drop 2 . reverse $" ++
                    "splitPath (dropFileName bin_path)")
            ]

    let autogen_dir = autogenModulesDir lbi
    let verbosity = fromFlag (buildVerbosity bf)
    let dynpaths_fname = autogen_dir </> dynpaths_module <.> "hs"
    createDirectoryIfMissingVerbose verbosity True autogen_dir
    rewriteFile dynpaths_fname contents

    -- running default hook
    (buildHook simpleUserHooks) pd lbi uh bf


main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
    buildHook = pathsHook,
    postBuild = bundleHook
    }
