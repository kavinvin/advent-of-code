module Day1Part2 (solution) where

import Data.List (sort)
import Data.List.Split (splitOn)

-- | Day 1: Calorie Counting
-- | https://adventofcode.com/2022/day/1
-- >>> solution "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
-- "45000"
solution :: String -> String
solution input = show max3Calories
  where max3Calories = sumTop3 . fmap caloriesPerElf . fmap lines . splitOn elfSeparator $ input
        caloriesPerElf = sum . fmap read
        elfSeparator = "\n\n"
        sumTop3 = sum . take 3 . reverse . sort
