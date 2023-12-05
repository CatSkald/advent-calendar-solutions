module Main where

import Control.Arrow
import Data.List
import Data.List.Split

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

parseCard :: String -> ([Int], [Int])
parseCard =
    splitOneOf ":|"
    >>> drop 1
    >>> map (words >>> map read)
    >>> tuplify

calculateCardWorth :: [Int] -> Int
calculateCardWorth [] = 0
calculateCardWorth [x] = 1
calculateCardWorth (x : xs) = 2 * calculateCardWorth xs

findWinningNumbers :: ([Int], [Int]) -> [Int]
findWinningNumbers (winners, chosen) =
    winners `intersect` chosen

calculateWorth :: [String] -> [Int]
calculateWorth =
  map $ calculateCardWorth.findWinningNumbers.parseCard

main = do
  fileLines <- fmap lines(readFile "../input/4.input.txt")
  print $ sum $ calculateWorth fileLines
