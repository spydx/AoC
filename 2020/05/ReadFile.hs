module ReadFile where

import System.IO (hGetContents, openFile, IOMode(ReadMode))

readfile :: FilePath -> IO [String]
readfile file = do 
   handle <- openFile file ReadMode
   content <- hGetContents handle
   let list = lines content
   return list 
