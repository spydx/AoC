module AoC01 where

import System.IO (hGetContents, openFile, IOMode(ReadMode))
import ReadFile
import System.Directory ( doesFileExist )



main = do
   let file = "01.txt"
   exist <- doesFileExist file 
   if exist then do
      c <- readfile file
      let intlist = [ read x::Int | x <- c]
      let res = product (head (solve intlist))
      print ("One: " ++ show res)
      let res = product (head (solve3 intlist))
      print ("Two: " ++ show res)

   else 
      print "no file found"

-- [1755,1668,837,1687,1901,1765,1687,1963,1945,1791,1688, 265]

solve :: [Int] -> [[Int]]
solve xs = [ [x,y] | x <- xs , y <- xs, verify x y ]
verify :: Int -> Int -> Bool
verify x y = sum [x,y] == 2020


solve3 :: (Eq a, Num a) => [a] -> [[a]]
solve3 xs = [[x,y,z] | x <- xs, y <- xs , z <- xs, verify3 x y z]
verify3 :: (Eq a, Num a) => a -> a -> a -> Bool
verify3 x y z = sum [x,y,z] == 2020