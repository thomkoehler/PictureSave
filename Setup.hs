
import Distribution.Simple(defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils(rewriteFile)
import Distribution.Package(packageVersion)
import Distribution.Simple.BuildPaths(autogenModulesDir)
import System.FilePath((</>))
import Data.Version(showVersion)


generateVersionModule pkg lbi = do
   let dir = autogenModulesDir lbi
   let version = packageVersion pkg

   rewriteFile (dir </> "Version.hs") $ unlines
      [
         "module Version where",
         "version :: String",
         "version = \"" ++ showVersion version ++ "\""
      ]


myBuildHook pkg lbi hooks flags = do
   generateVersionModule pkg lbi
   buildHook simpleUserHooks pkg lbi hooks flags


main = defaultMainWithHooks simpleUserHooks
   {
      buildHook=myBuildHook
   }
