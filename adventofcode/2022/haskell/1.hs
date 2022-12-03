module Main where
  
import Data.List.Split
import Data.List

makeInteger :: [String] -> [Int]
makeInteger = map read

getScore :: ([String], Int) -> Int
getScore (fileLines, topX) = sum.take topX $ reverse.sort $ map (sum.makeInteger) (splitWhen (=="") fileLines)

main = do
  fileLines <- fmap lines(readFile "../input/1input.txt")
  print $ getScore (fileLines, 1)
  print $ getScore (fileLines, 3)
