module Main where

import Data.List

parseMove :: [String] -> (Int, Int, Int)
parseMove ["move", qty, "from", a, "to", b] = (read qty, read a, read b)

parseMoves :: [String] -> [(Int, Int, Int)]
parseMoves x = map (parseMove.words) $ drop 1 $ dropWhile (/="") x

parseCrateRows' :: [Char] -> [Char]
parseCrateRows' [] = []
parseCrateRows' [x] = [x]
parseCrateRows' [_, x, _] = [x]
parseCrateRows' (_:x:_:' ':xs) = x : parseCrateRows' xs

parseCrates :: [String] -> [String]
parseCrates x = map parseCrateRows' $ init $ takeWhile (/="") x

-- TODO Finish me
main = do
  fileLines <- fmap lines(readFile "../input/5example.txt")
  print (parseMoves fileLines, parseCrates fileLines)
