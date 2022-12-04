{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Data.Typeable (Typeable)
import Data.Data (Data)
import System.Console.CmdArgs
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Data.Map as M

data SolutionArgs = SolutionArgs { day :: Int, part :: Int } deriving (Show, Data, Typeable)

solutionArgs = SolutionArgs { day = def &= help "Day of Advent of Code", part = def }
         &= summary "Advent of Code solutions"

solution :: SolutionArgs -> (String -> String)
solution (SolutionArgs day part) = solutionMap M.! (day, part)
  where solutionMap = M.fromList [ ((1, 1), Day1.solution)
                                 , ((1, 2), Day1.solution2)
                                 , ((2, 1), Day2.solution)
                                 , ((3, 1), Day3.solution) ]

-- Receive input from stdin
-- Tips for macOS: pbpaste | stack run -- --day 1
main :: IO ()
-- main = interact Day3.solution
main = do
  parsedArgs <- cmdArgs solutionArgs
  interact (solution parsedArgs)
