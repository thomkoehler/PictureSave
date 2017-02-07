
module Options
(
   getOptions,
   printUsage,
   Options(..)
)
where

import System.Console.GetOpt
import Data.Maybe


data Options = Options
   {
      srcDir :: !FilePath,
      targetDir :: !FilePath,
      onlyView :: !Bool,
      targetFileFormat :: !String,
      help :: !Bool
   }
   deriving(Show)


defaultOptions :: Options
defaultOptions = Options "." "." False "Private_%0Y%0m%0d_%0H%0M%0S.jpg" False


options :: [OptDescr (Options -> Options)]
options =
   [
      Option "s" [] (OptArg ((\f opts -> opts { srcDir = f }) . fromMaybe "srcDir") "DIR") "source directory (default .)",
      Option "t" [] (OptArg ((\f opts -> opts { targetDir = f }) . fromMaybe "targetDir") "DIR") "target directory (default .)",
      Option "f" [] (OptArg ((\f opts -> opts { targetFileFormat = f }) . fromMaybe "targetFileFormat") "Format") "target file format (default Private_%0Y%0m%0d_%0H%0M%0S.jpg)",
      Option "h" ["help"] (NoArg (\opts -> opts { help = True })) "show usage",
      Option "v" ["view"] (NoArg (\opts -> opts { onlyView = True })) "only view actions"
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
