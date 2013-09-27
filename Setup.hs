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
        "FlyingSheepBattles"
        Nothing
        Nothing -- Build a default Info.plist for the icon.
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

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
    postBuild = bundleHook
    }
