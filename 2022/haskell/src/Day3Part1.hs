module Day3Part1 (solution) where

import Data.List (intersect, nub)
import Data.Map qualified as M

-- | Day 3: Rucksack Reorganization
-- | https://adventofcode.com/2022/day/3
-- >>> solution "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
-- "157"
solution :: String -> String
solution = show . sum . fmap calculate . lines
  where
    calculate line =
      let (prior, latter) = splitHalf line
       in sum $ fmap priority $ nub $ prior `intersect` latter

-- |
-- >>> priority 'z'
-- 26
priority :: Char -> Int
priority c = mapper M.! c
  where
    mapper = lowerCharMapper `M.union` upperCharMapper
    lowerCharMapper = M.fromList (zip ['a' .. 'z'] [1 .. 26])
    upperCharMapper = M.fromList (zip ['A' .. 'Z'] [27 .. 54])

-- |
-- >>> splitHalf "vJrwpWtwJgWrhcsFMMfFFhFp"
-- ("vJrwpWtwJgWr","hcsFMMfFFhFp")
splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt halfLength xs
  where
    halfLength = length xs `div` 2
