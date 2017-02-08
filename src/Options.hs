
module Options
(
   getOptions,
   printUsage,
   Options(..)
)
where

import System.Console.GetOpt
import Data.Maybe
import Text.Printf


data Options = Options
   {
      srcDir :: !FilePath,
      targetDir :: !FilePath,
      onlyView :: !Bool,
      targetFileFormat :: !String,
      help :: !Bool,
      viewVersion :: !Bool
   }
   deriving(Show)


defaultOptions :: Options
defaultOptions = Options
   {
      srcDir = ".",
      targetDir = ".",
      onlyView = False,
      targetFileFormat = "Private_%0Y%0m%0d_%0H%0M%0S.jpg",
      help = False,
      viewVersion = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [
      Option "s" [] (OptArg ((\f opts -> opts { srcDir = f }) . fromMaybe "srcDir") "DIR")
         (printf "source directory (default %s)" (srcDir defaultOptions)),

      Option "o" [] (OptArg ((\f opts -> opts { targetDir = f }) . fromMaybe "targetDir") "DIR")
         (printf "output directory (default %s)" (targetDir defaultOptions)),

      Option "f" [] (OptArg ((\f opts -> opts { targetFileFormat = f }) . fromMaybe "targetFileFormat") "Format")
         (printf "target file format (default %s)" (targetFileFormat defaultOptions)),

      Option "h" ["help"] (NoArg (\opts -> opts { help = True })) "show usage",
      Option "a" ["actions"] (NoArg (\opts -> opts { onlyView = True })) "only view actions",
      Option "v" ["version"] (NoArg (\opts -> opts { viewVersion = True })) "view version"
   ]


usageHeader :: String
usageHeader = "Usage: PictureSave [OPTION...]"


getOptions :: [String] -> Options
getOptions argv =
   case getOpt Permute options argv of
      (o, _, []) -> foldl (flip id) defaultOptions o
      (_ ,_, errs) -> error $ concat errs ++ usageInfo usageHeader options


printUsage :: IO ()
printUsage = putStrLn $ usageInfo usageHeader options
