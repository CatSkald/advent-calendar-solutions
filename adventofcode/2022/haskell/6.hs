module Main where
  
import Data.List

getStart :: Int -> Int -> String -> Int
getStart current amount x =
  if areUnique $ take amount x
    then current + amount
    else getStart (current + 1) amount (tail x)

areUnique :: (Eq a) => [a] -> Bool
areUnique [] = True
areUnique (x:xs) = x `notElem` xs && areUnique xs

main = do
  fileLines <- fmap lines(readFile "../input/6input.txt")
  print $ map (getStart 0 4) fileLines
  print $ map (getStart 0 14) fileLines
