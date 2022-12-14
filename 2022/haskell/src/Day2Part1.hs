module Day2Part1 (solution) where

import Data.List.Split (splitOn)

-- | Day 2: Rock Paper Scissors
-- | https://adventofcode.com/2022/day/2
-- >>> solution "A Y\nB X\nC Z"
-- "15"
solution :: String -> String
solution input = show $ sum scores
  where
    scores = (fmap roundScore . fmap extractRoundHandShape . lines) input
    extractRoundHandShape line = let [opponent, self] = splitOn " " line in (read self, read opponent)

data HandShape = Rock | Paper | Scissors deriving (Eq, Show)

-- |
-- >>> (read "A") :: HandShape
-- Rock
instance Read HandShape where
  readsPrec _ input = [(handShape, rest)]
    where
      (first : rest) = input
      handShape = case first of
        'A' -> Rock
        'B' -> Paper
        'C' -> Scissors
        'X' -> Rock
        'Y' -> Paper
        'Z' -> Scissors

-- |
-- >>> handShapeScore Rock
-- 1
handShapeScore :: HandShape -> Int
handShapeScore Rock = 1
handShapeScore Paper = 2
handShapeScore Scissors = 3

-- |
-- >>> outcomeScore (Rock, Scissors)
-- 6
outcomeScore :: (HandShape, HandShape) -> Int
outcomeScore (self, opponent)
  | self == opponent = draw
  | (self, opponent) `elem` winningPairs = win
  | (self, opponent) `elem` losingPairs = lose
  where
    win = 6
    draw = 3
    lose = 0
    winningPairs = [(Rock, Scissors), (Paper, Rock), (Scissors, Paper)]
    losingPairs = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

-- |
-- >>> roundScore (Rock, Scissors)
-- 7
roundScore :: (HandShape, HandShape) -> Int
roundScore (self, opponent) = outcomeScore (self, opponent) + handShapeScore self
