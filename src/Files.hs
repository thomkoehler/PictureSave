
module Files
(
   listDir,
   exifTimeOriginal,
   createUniqueFilename
)
where

import System.Directory
import System.FilePath
import Control.Monad
import System.FilePath.Glob
import Data.Time.LocalTime
import Graphics.HsExif
import Data.List
import Data.Maybe


listDir :: String -> FilePath -> IO [FilePath]
listDir strPattern dir = do
   names <- getDirectoryContents dir
   let pattern = compile strPattern
   let fullNames = map (dir </>) $ filter (match pattern) names
   filterM doesFileExist fullNames


exifTimeOriginal :: FilePath -> IO (Maybe LocalTime)
exifTimeOriginal file = do
   exif <- parseFileExif file
   case exif of
      Right values -> return $ getDateTimeOriginal values
      _ -> return Nothing


createUniqueFilename :: [String] -> String -> String
createUniqueFilename fileNameList fileName = fromJust $ find (\fn -> not (elem  fn fileNameList)) $ fileName : map createFileName (zip (repeat fileName) [1..])
   where
      createFileName (name, num) = name ++ "_" ++ show num
