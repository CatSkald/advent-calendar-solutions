module Main where

getScore1 :: (Char, Char) -> Int
getScore1 (x, y) = case (x, y) of
  ('A', 'X') -> 3 + 1
  ('B', 'Y') -> 3 + 2
  ('C', 'Z') -> 3 + 3
  ('C', 'X') -> 6 + 1
  ('A', 'Y') -> 6 + 2
  ('B', 'Z') -> 6 + 3
  (_, 'X') -> 1
  (_, 'Y') -> 2
  (_, 'Z') -> 3

getScore2 :: (Char, Char) -> Int
getScore2 (x, y) = case (x, y) of
  ('A', 'X') -> 3
  ('B', 'X') -> 1
  ('C', 'X') -> 2
  ('A', 'Y') -> 3 + 1
  ('B', 'Y') -> 3 + 2
  ('C', 'Y') -> 3 + 3
  ('A', 'Z') -> 6 + 2
  ('B', 'Z') -> 6 + 3
  ('C', 'Z') -> 6 + 1

main = do
  fileLines <- fmap lines(readFile "../input/2input.txt")
  print $ sum $ map (getScore1.(\[x,y,z] -> (x,z))) fileLines
  print $ sum $ map (getScore2.(\[x,y,z] -> (x,z))) fileLines
