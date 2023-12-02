{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.List
import qualified Data.Text as T

trimGame :: T.Text -> [T.Text]
trimGame =
  T.replace "Game " ""
  >>> T.replace ":" ";"
  >>> T.splitOn "; "

parseGame :: [T.Text] -> (Int, [(Int, T.Text)])
parseGame (game : draws) =
  (read $ T.unpack game, concatMap parseDraws draws)

filterPossible :: [(Int, [(Int, T.Text)])] -> [(Int, [(Int, T.Text)])]
filterPossible [] = []
filterPossible (x : xs)
  | isPossibleGame $ snd x = x : filterPossible xs
  | otherwise = filterPossible xs

isPossibleGame :: [(Int, T.Text)] -> Bool
isPossibleGame [] = True
isPossibleGame [x] = isPossibleDraw x
isPossibleGame (x : xs) = isPossibleDraw x && isPossibleGame xs

isPossibleDraw :: (Int, T.Text) -> Bool
isPossibleDraw (x, "red") = x <= 12
isPossibleDraw (x, "green") = x <= 13
isPossibleDraw (x, "blue") = x <= 14
isPossibleDraw x = False

oneOr :: Int -> Int
oneOr 0 = 1
oneOr x = x

calculateMinimumColorPower :: T.Text -> [(Int, T.Text)] -> Int
calculateMinimumColorPower c =
  filter ((==c).snd) >>> oneOr.maximum.map fst

calculateMinimumSetPower :: [(Int, T.Text)] -> Int
calculateMinimumSetPower x =
  calculateMinimumColorPower "green" x
  * calculateMinimumColorPower "blue" x
  * calculateMinimumColorPower "red" x

parseDraws :: T.Text -> [(Int, T.Text)]
parseDraws =
  T.splitOn ", " >>> map parseColor

parseColor :: T.Text -> (Int, T.Text)
parseColor =
  T.splitOn " "
  >>> (\[x, y] -> (read $ T.unpack x, y))

findPossibleGames :: [String] -> [Int]
findPossibleGames x =
  map fst $ filterPossible $ map (T.pack >>> trimGame >>> parseGame) x

findMinimumSetPowers :: [String] -> [Int]
findMinimumSetPowers =
  map (T.pack >>> trimGame >>> parseGame >>> calculateMinimumSetPower.snd)

main = do
  fileLines <- fmap lines(readFile "../input/2.input.txt")
  print $ sum $ findPossibleGames fileLines
  print $ sum $ findMinimumSetPowers fileLines
