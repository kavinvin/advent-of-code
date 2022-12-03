module Main (main) where

import qualified Day1
import qualified Day2

-- Receive input from stdin
-- Tips for macOS: pbpaste | stack run
main :: IO ()
main = interact Day2.solution
