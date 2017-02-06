module Main where

import Files
import Graphics.HsExif
import System.FilePath.Windows
import Data.Time.LocalTime
import Data.Time.Format


jpgExt :: String
jpgExt = "*.jpg"


main :: IO ()
main = copyJpgs "./Temp/src" "./Temp/target"


mbLocalTimeToFilename :: Maybe LocalTime -> String -> String
mbLocalTimeToFilename Nothing oldFileName = oldFileName
mbLocalTimeToFilename (Just lt) _ = formatTime defaultTimeLocale "%0Y" lt


copyJpgs :: FilePath -> FilePath -> IO ()
copyJpgs srcDir targetDir = do
   srcFiles <- listDir jpgExt srcDir
   srcFileExifInfos <- mapM exifTimeOriginal srcFiles
   let srcFileNames = map takeFileName srcFiles

   print $ zipWith mbLocalTimeToFilename srcFileExifInfos srcFileNames

   targetFiles <- listDir jpgExt targetDir
   let targetFileNames = map takeFileName targetFiles

   return ()
