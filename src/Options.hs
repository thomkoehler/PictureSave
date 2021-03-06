
module Options
(
   getOptions,
   printUsage,
   Options(..)
)
where

import System.Console.GetOpt
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
      targetFileFormat = "%0y%0m%0d_%0H%0M%0S.JPG",
      help = False,
      viewVersion = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [
      Option "i" ["indir"] (ReqArg (\f opts -> opts { srcDir = f }) "inputDir") (printf "input directory (default %s)" (srcDir defaultOptions)),
      Option "o" ["outdir"] (ReqArg (\f opts -> opts { targetDir = f }) "outDir") (printf "output directory (default %s)" (targetDir defaultOptions)),
      Option "f" ["fileFormat"] (ReqArg (\f opts -> opts { targetFileFormat = f }) "format") (printf "target file format (default %s)" (targetFileFormat defaultOptions)),
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
