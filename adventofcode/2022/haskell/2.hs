module Main where

getScore1 :: [Char] -> Int
getScore1 x = case x of
  ['A', _, 'X'] -> 3 + 1
  ['B', _, 'Y'] -> 3 + 2
  ['C', _, 'Z'] -> 3 + 3
  ['C', _, 'X'] -> 6 + 1
  ['A', _, 'Y'] -> 6 + 2
  ['B', _, 'Z'] -> 6 + 3
  [_, _, 'X'] -> 1
  [_, _, 'Y'] -> 2
  [_, _, 'Z'] -> 3

getScore2 :: [Char] -> Int
getScore2 x = case x of
  ['A', _, 'X'] -> 3
  ['B', _, 'X'] -> 1
  ['C', _, 'X'] -> 2
  ['A', _, 'Y'] -> 3 + 1
  ['B', _, 'Y'] -> 3 + 2
  ['C', _, 'Y'] -> 3 + 3
  ['A', _, 'Z'] -> 6 + 2
  ['B', _, 'Z'] -> 6 + 3
  ['C', _, 'Z'] -> 6 + 1

main = do
  fileLines <- fmap lines(readFile "../input/2input.txt")
  print $ sum $ map getScore1 fileLines
  print $ sum $ map getScore2 fileLines
