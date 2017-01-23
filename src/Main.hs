module Main where

import Files
import Graphics.HsExif

main :: IO ()
main = do
  files <- listDir "*.jpg" "C:\\Users\\koehler\\Downloads"
  exifs <- mapM exifTimeOriginal files
  print exifs
