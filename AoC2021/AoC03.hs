module AoC03 where

import Modules.ReadFile (readfile)
import System.Directory ( doesFileExist )
import Data.List (transpose)
import Data.Char (ord, digitToInt)

main :: IO ()
main = do 
    let file = "aoc03test.txt"
    exists <- doesFileExist file
    if exists then do
        content <- readfile file        
        let pc = processcontent content
        --print pc
        print $ task1 pc

    else 
        print "No file found"

processcontent :: [String] -> [[Int]]
processcontent c =  [ [ digitToInt x   | x <- xs ] | xs <- transpose c]

--task1 :: [[Int]] -> ([Int],[Int])
task1 = result 
    where
        result content = (getgamma content [], epsilon (getgamma content []))
        getgamma :: [[Int]] -> [(Int,Int)] -> [Int]
        getgamma content l = gamma $ foldl (\ l c -> l ++ majorge c) l content
        gamma :: [(Int, Int)] -> [Int]
        gamma = map fst
        epsilon :: [Int] -> [Int]
        epsilon [] = []
        epsilon (g:gamma) 
                | g == 1 = 0 : epsilon gamma
                | otherwise = 1 : epsilon gamma
        
majorge :: [Int] -> [(Int,Int)]
majorge c = [tuple c (sum [ x | x <- c , x == 1])]
    where
        tuple :: [Int] -> Int -> (Int, Int)
        tuple c x = major (x, length c - x) 

major :: (Int, Int) -> (Int, Int)
major (x,y)
    | x < y = (0,1)
    | otherwise = (1,0)

