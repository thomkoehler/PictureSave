module Main where

import Files
import Graphics.HsExif
import System.FilePath.Windows


jpgExt :: String
jpgExt = "*.jpg"


main :: IO ()
main = copyJpgs "./Temp/src" "./Temp/target"


copyJpgs :: FilePath -> FilePath -> IO ()
copyJpgs srcDir targetDir = do
   srcFiles <- listDir jpgExt srcDir
   srcFileExifInfos <- mapM exifTimeOriginal srcFiles
   let srcFileNames = map takeFileName srcFiles

   targetFiles <- listDir jpgExt targetDir
   let targetFileNames = map takeFileName targetFiles

   return ()
