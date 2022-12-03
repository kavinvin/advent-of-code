module Day1 (solution) where

import Data.List.Split (splitOn)

-- | Day 1: Calorie Counting
-- >>> solution "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
-- "24000"
solution :: String -> String
solution input = show $ maximum caloriesForEachElf
    where caloriesForEachElf = fmap sum . (fmap . fmap) (read :: String -> Int) . fmap lines . splitOn "\n\n" $ input
