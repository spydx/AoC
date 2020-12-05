module AoC02 where
import ReadFile ( readfile )
import System.Directory ( doesFileExist )
import Data.Char (isDigit)

main :: IO ()
main = do
   let file = "02.txt"
   exist <- doesFileExist file 
   if exist then do
      c <- readfile file
      let list = [ validate r | r <-  c]
      let res = length [ v | v <- list, v]
      let p = [ validate2 r | r <- c]
      let res2 = length [v | v <- p, v]
      print ("One: " ++ show res)
      print ("Two: " ++ show res2)
   else 
      print "no file found"

-- ["5-9", "g:", "ggccggmgn"]

validate :: String -> Bool
validate str = password (words str)

validate2 :: String -> Bool
validate2 str = password2 (words str)

password :: [String] -> Bool
password str = 
   validatePassword (getstart (head str)) (getend (head str)) ( head (str !! 1)) (last str)

getstart :: String -> Int
getstart xs = read (parse xs)::Int

getend :: String -> Int
getend xs = read (reverse (parse (reverse xs)))::Int

validatePassword :: Eq a => Int -> Int -> a -> [a] -> Bool
validatePassword x y c str = between x y $ count c str

between :: Ord a => a -> a -> a -> Bool
between x y r = x <= r && r <= y

count :: Eq a => a -> [a] -> Int
count c str = length [s | s <- str, s == c]


-- validate "11-16 l: llllqllllllllflq" -> True
-- need to parse this "11-16"

parse :: String -> String
parse [] = []
parse (x:xs) 
   | isDigit x = x : parse xs
   | otherwise = []


-- 1-3 a: abcde - True
-- 1-3 b: cdefg - False

password2 :: [String] -> Bool
password2 str = policy (getstart (head str)) (getend (head str)) ( head (str !! 1)) (last str)

-- "2-9 c: cbccccccc" --> True
-- "2-9 c: ccccccccb" --> True
-- "2-9 c: ccccccccc" --> False
policy :: Eq a => Int -> Int -> a -> [a] -> Bool
policy x y c str = if ((str!!(x-1)) == c) then 
                           ((str!!(y-1)) /= c) 
                   else 
                        ((str!!(y-1)) == c) 