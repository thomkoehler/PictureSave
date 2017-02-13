
module Files
(
   listDir,
   exifTimeOriginal,
   createUniqueFileNames
)
where

import System.Directory
import System.FilePath
import Control.Monad
import Data.Time.LocalTime
import Graphics.HsExif
import Data.List
import Data.Maybe
import Data.Char


listDir :: String -> FilePath -> IO [FilePath]
listDir ext dir = do
   let lowerExt = map toLower ext
   names <- getDirectoryContents dir
   let filteredNames = filter (isSuffixOf lowerExt . map toLower) names
   let fullNames = map (dir </>) filteredNames
   filterM doesFileExist fullNames


exifTimeOriginal :: FilePath -> IO (Maybe LocalTime)
exifTimeOriginal file = do
   exif <- parseFileExif file
   case exif of
      Right values -> return $ getDateTimeOriginal values
      _ -> return Nothing


createUniqueFilename :: [String] -> String -> String
createUniqueFilename fileNameList fileName =
   fromJust $ find (\fn -> not (elem  fn fileNameList)) $ fileName : map createFileName (zip (repeat fileName) [(1 :: Int)..])
   where
      createFileName (name, num) =
         let
            ext = takeExtension name
            baseName = dropExtension name
         in
            baseName ++ "_" ++ show num ++ ext


createUniqueFileNames :: [String] -> [String] -> [String]
createUniqueFileNames existNames (name: restNames) =
   let
      uniqueFileName = createUniqueFilename existNames name
   in
      uniqueFileName : createUniqueFileNames (uniqueFileName:existNames) restNames

createUniqueFileNames _ _ = []
