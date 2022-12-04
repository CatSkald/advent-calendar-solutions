module Main where
  
import Data.Char
import Data.List
import Data.List.Split

getAsInt :: String -> Int
getAsInt = read

getValues :: [[String]] -> (Int, Int, Int, Int)
getValues [[a, b], [c, d]] = (getAsInt a, getAsInt b, getAsInt c, getAsInt d)

isSubSet :: (Int, Int, Int, Int) -> Bool
isSubSet (a, b, c, d) = (a <= c && b >= d) || (c <= a && d >= b)

isOverlap :: (Int, Int, Int, Int) -> Bool
isOverlap (a, b, c, d) = (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d >= a && d <= b)

tuplify :: [String] -> [(Int, Int, Int, Int)]
tuplify = map (getValues.(map (splitWhen (=='-')).splitWhen (==',')))

getSolution1 :: [String] -> Int
getSolution1 x = length $ filter isSubSet $ tuplify x

getSolution2 :: [String] -> Int
getSolution2 x = length $ filter isOverlap $ tuplify x

main = do
  fileLines <- fmap lines(readFile "../input/4input.txt")
  print $ getSolution1 fileLines
  print $ getSolution2 fileLines
