module Main where

import           Enumerate
import           TopologyData

main :: IO ()
main = print $ map showField $ head (enumerateModelsByTopology BC)
