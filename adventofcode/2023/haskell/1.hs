{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Text hiding (map, lines, last)

filterDigits :: String -> String
filterDigits [] = []
filterDigits (x : xs)
  | isDigit x = x : filterDigits xs
  | otherwise = filterDigits xs

findCalibrationValue :: String -> Int
findCalibrationValue []       = 0
findCalibrationValue [x]      = read [x, x]
findCalibrationValue [x, y]   = read [x, y]
findCalibrationValue (x : xs) = read [x, last xs]

findSolution1 :: [String] -> Int
findSolution1 x = sum $ map (findCalibrationValue.filterDigits) x

main = do
  fileLines <- fmap lines(readFile "../input/1.input.txt")
  print $ findSolution1 fileLines
