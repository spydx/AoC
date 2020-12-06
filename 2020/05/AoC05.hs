module AoC05 where

import System.Directory ( doesFileExist )

main :: IO ()
main = do
   let file = "05.txt"
   exist <- doesFileExist file 
   if exist then do
      c <- readFile file
      print (lines c)
     
   else 
      print "no file found"




rowrange :: (Integral a1, Integral b, RealFrac a2, RealFrac a3) 
            => a3 -> a2 -> Char -> (a1, b)
rowrange l u c = case c of
               'B' ->  ( half u, ceiling u)
               'F' ->  ( ceiling l , half u)
               _ -> error "something whent wrong"
      
half :: (RealFrac a, Integral b) => a -> b
half u = ceiling (u/2)


rise u i = ceiling (u-(u/(2*i)))
seatrange :: (RealFrac a1, RealFrac a2, Integral a3, Integral b) 
               => a1 -> a2 -> Char -> (a3, b)
seatrange l u s = case s of
               'L' -> (ceiling l, half u)
               'R' -> (half u, ceiling u)
               _ -> error "Badd INPUT"


range l u i str = case str of
               ('B':left) -> 
                  let lower = rise u i
                      upper = ceiling u 
                  in ( lower , upper )  : range lower upper (i+1) left
               --('F':left) -> (ceiling l, half u) : range l (u-(u/(2*i))) (i+1) left

               --[] -> []
