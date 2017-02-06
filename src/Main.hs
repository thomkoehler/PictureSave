module Main where

import System.FilePath.Windows
import System.Directory
import Data.Time.LocalTime
import Data.Time.Format
import Data.Foldable
import Text.Printf
import System.Environment
import Control.Monad

import Files
import Options


jpgExt :: String
jpgExt = "*.jpg"


main :: IO ()
main = do
   args <- getArgs
   let options = getOptions args
   if help options
      then printUsage
      else copyJpgs options


mbLocalTimeToFilename :: String -> Maybe LocalTime -> String -> String
mbLocalTimeToFilename _ Nothing oldFileName = oldFileName
mbLocalTimeToFilename fileFormat (Just lt) _ = formatTime defaultTimeLocale fileFormat lt


copyJpgs :: Options -> IO ()
copyJpgs (Options sd td ov tff _) = do
   srcFiles <- listDir jpgExt sd
   srcFileExifInfos <- mapM exifTimeOriginal srcFiles
   let srcFileNames = map takeFileName srcFiles
   let newSrcFileNames = zipWith (mbLocalTimeToFilename tff) srcFileExifInfos srcFileNames
   targetFiles <- listDir jpgExt td
   let targetFileNames = map takeFileName targetFiles
   let uniqueSrcFileNames = createUniqueFileNames targetFileNames newSrcFileNames
   let fullUniqueFileNames = map (td </>) uniqueSrcFileNames
   forM_ (zip srcFiles fullUniqueFileNames) $ \(srcFile, targetFile) -> do
      printf "copy from '%s' to '%s'\n" srcFile targetFile
      unless ov $ copyFileWithMetadata srcFile targetFile
