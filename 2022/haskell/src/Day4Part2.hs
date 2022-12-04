module Day4Part2 (solution) where

import Data.List.Split (splitOn)
import Data.List (isSubsequenceOf, length, intersect)

count f = length . filter f

overlap xs ys = (length $ xs `intersect` ys) > 0

-- |
-- >>> solution "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
-- "4"
solution :: String -> String
solution = show . count (== True) . fmap compute . lines
  where splitLine line = let [first, second] = splitOn "," line in (toRange first, toRange second)
        toRange word = let [from, to] = splitOn "-" word in [(read from :: Int)..(read to :: Int)]
        compute line = let (first, second) = splitLine line in first `overlap` second
