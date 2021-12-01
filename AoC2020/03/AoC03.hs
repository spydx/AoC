module AoC03 where
import System.IO (hGetContents, openFile, IOMode(ReadMode))
import System.Directory ( doesFileExist )


readfile :: FilePath -> IO [String]
readfile file = do 
   handle <- openFile file ReadMode
   content <- hGetContents handle
   let list = lines content
   return list 



main :: IO ()
main = do
   let file = "03.txt"
   exist <- doesFileExist file
   if exist then do
         c <- readfile file
         putStrLn (concat c)
   else 
      print "no file found"