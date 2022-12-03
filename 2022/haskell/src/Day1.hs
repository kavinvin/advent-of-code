module Day1 (solution) where

import Data.List.Split (splitOn)

-- | Day 1: Calorie Counting
-- | https://adventofcode.com/2022/day/1
-- >>> solution "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
-- "24000"
solution :: String -> String
solution input = show maxCalories
  where maxCalories = maximum . fmap caloriesPerElf . fmap lines . splitOn elfSeparator $ input
        caloriesPerElf = sum . fmap read
        elfSeparator = "\n\n"
