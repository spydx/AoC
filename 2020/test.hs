module Main where

import Prelude hiding (readFile, length)
import qualified Text.Megaparsec
import Text.Megaparsec (many, eof, takeWhileP)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isLetter)
import qualified Data.Text
import Data.Text (Text, index, length)
import Data.Text.IO (readFile)
import qualified Data.Void
import Data.Foldable (foldl')

data PW = PW
  Int   -- first number
  Int   -- second number
  Char  -- char
  Text  -- password
  deriving(Show)

type PWParser a = Text.Megaparsec.Parsec Data.Void.Void Text a

parseLine :: PWParser PW
parseLine = do
  n1 <- decimal
  _ <- char '-'
  n2 <- decimal
  _ <- char ' '
  c <- letterChar
  _ <- char ':'
  _ <- char ' '
  pw <- takeWhileP Nothing isLetter
  _ <- char '\n'
  return $ PW n1 n2 c pw

parseFile :: PWParser [PW]
parseFile = many parseLine <* eof

countP :: Foldable f => (a -> Bool) -> f a -> Int
countP p = foldl' (\count x -> if p x then count + 1 else count) 0

count :: (Eq a, Foldable f) => a -> f a -> Int
count x = countP (==x)

check1 :: PW -> Bool
check1 (PW i1 i2 c pw) = let
    n = count c $ Data.Text.unpack pw
  in
    i1 <= n && n <= i2

part1 :: [PW] -> IO ()
part1 pws = putStrLn . show $ countP check1 pws

checkIndex :: Int -> Text -> Char -> Bool
checkIndex i t c = (i <= length t) && (index t (i-1) == c)

check2 :: PW -> Bool
check2 (PW i1 i2 c pw) = case (checkIndex i1 pw c, checkIndex i2 pw c) of
  (True, True) -> False
  (True, False) -> True
  (False, True) -> True
  (False, False) -> False

part2 :: [PW] -> IO ()
part2 pws = putStrLn . show $ countP check2 pws

main :: IO ()
main = do
  let fn = "input"
  input <- readFile fn
  let (Right pws) = Text.Megaparsec.parse parseFile fn input
  part1 pws
  part2 pws