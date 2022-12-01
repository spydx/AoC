module AoC01 where

import Modules.ReadFile (readfile)
import System.Directory ( doesFileExist )

main :: IO ()
main = do 
    let file = "aoc01.txt"
    exists <- doesFileExist file
    if exists then do
        content <- readfile file
        let processedcontent = [ read x::Int | x <- content]
        print $ countincresses processedcontent
        print $ treesearchincresses processedcontent
    else 
        print "No file found"

countincresses :: [Int] -> Int
countincresses xs = depthchange (head xs) xs 0

treesearchincresses :: [Int] -> Int
treesearchincresses xs = depthchangewindow (findwindow xs) (tail xs) 0

depthchange :: Int -> [Int] -> Int -> Int
depthchange _ [] count = count
depthchange current (x:xs) count 
    | x > current = depthchange x xs (count + 1)
    | otherwise = depthchange x xs count

depthchangewindow :: Int -> [Int] -> Int -> Int
depthchangewindow _ [] count = count
depthchangewindow current xs count
    | length xs >= 3 =  if findwindow xs > current then
                            depthchangewindow (findwindow xs) (tail xs) (count + 1)
                        else
                            depthchangewindow (findwindow xs) (tail xs) count
    | otherwise = count
        
findwindow :: [Int] -> Int
findwindow xs 
    | length xs >= 3 = sum (take 3 xs)
    | otherwise = error "Ivalid length to sum over"
