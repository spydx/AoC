module AoC02 where

import Modules.ReadFile (readfile)
import System.Directory ( doesFileExist )

main :: IO ()
main = do 
    let file = "aoc02.txt"
    exists <- doesFileExist file
    if exists then do
        content <- readfile file
        let pc = processcontent content
        print $ position pc 0 0
        print $ positionaim pc 0 0 0

    else 
        print "No file found"


processcontent :: [String] -> [(String, Int)]
processcontent content = [ tuples (words x)| x <- content ]
    where
        tuples :: [String] -> (String, Int)
        tuples x = (head x, read (last x))


position :: [(String, Int)] -> Int -> Int -> Int
position [] x y = x * y
position (p:path) x y = case p of
            ("forward", x') -> position path (x + x') y
            ("down", y') -> position path x (y + y')
            ("up", y') -> position path x (y - y')
            _ -> error "Unexpected token"

positionaim :: [(String, Int)] -> Int -> Int -> Int -> Int
positionaim [] x y a =  x * y
positionaim (p:path) x y a = case p of
            ("forward", x') -> positionaim path (x + x') (y + x' * a) a
            ("down", y') -> positionaim path x y (a + y')
            ("up", y') -> positionaim path x y (a - y')
            _ -> error "Unexpected token"
