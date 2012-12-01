import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "SDLTemplate"
                  (Just "SDLTemplate.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  -- list or resources (to be copied to .app/Contents/Resources)
                  ["image.png"]
                  [] -- No other binaries.
                  -- collect non-standard dynamic libraries (to be copied to .app/Contents/Frameworks)
                  ChaseWithDefaults
          ]
