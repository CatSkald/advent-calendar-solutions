module Main where

import Data.List
import Data.Char

getCompartments :: String -> (String, String)
getCompartments x = splitAt (div (length x) 2) x

getWrongItem :: (String, String) -> [Char]
getWrongItem (x, y) = nub $ x `intersect` y

getPriority :: Char -> Int
getPriority x
  | ord x < ord 'a' = ord x - ord 'A' + 27
  | otherwise = ord x - ord 'a' + 1

getSolution1 :: [String] -> Int
getSolution1 x = sum $ map ((sum.map getPriority).(getWrongItem.getCompartments)) x

getCommonItem' :: [String] -> [String]
getCommonItem' [x, y, z] = [nub $ x `intersect` y `intersect` z]
getCommonItem' (x:y:z:xs) = getCommonItem' [x, y, z] ++ getCommonItem' xs

getSolution2 :: [String] -> Int
getSolution2 x = sum $ map (getPriority.head) (getCommonItem' x)

main = do
  fileLines <- fmap lines(readFile "../input/3input.txt")
  print $ getSolution1 fileLines
  print $ getSolution2 fileLines
