import Distribution.MacOSX
import Distribution.Simple

import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import System.FilePath
import Control.Monad (when, liftM)
import Distribution.Simple.Utils (matchFileGlob)
import Data.Maybe (fromJust)


applist :: [FilePath] -> [MacApp]
applist resources = [
    MacApp
        "SDLTemplate"
        (Just "SDLTemplate.icns")
        Nothing -- Build a default Info.plist for the icon.
        -- list or resources (to be copied to .app/Contents/Resources)
        resources
        [] -- No other binaries.
        -- collect non-standard dynamic libraries (to be copied to .app/Contents/Frameworks)
        ChaseWithDefaults
    ]


bundleHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
bundleHook args bf pd lbi = do
    let ddir = dataDir pd
    let bundleall = fromJust $ lookup (FlagName "bundleall") (configConfigurationsFlags $ configFlags lbi)

    when (ddir /= "") (ioError (userError "Non-empty data-dir is not supported"))

    resources <- ((liftM concat) . (mapM matchFileGlob)) (dataFiles pd)
    let resources_to_bundle = if bundleall then resources else []

    appBundleBuildHook (applist resources_to_bundle) args bf pd lbi


main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
    postBuild = bundleHook
    }
