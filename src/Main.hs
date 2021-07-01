module Main where

import           Enumerate
import           Formatting
import           TopologyData
import           Overlap

-- Read in tree-level models from Python output
readModels :: IO [[String]]
readModels = do
  let fileName = "data/treelevelmodels.csv"
  contents <- readFile fileName
  let models = map (read :: String -> [String]) (lines contents)
  return models

main :: IO ()
main = putStrLn $ tabulateModels $ map snd $ overlap gMinus2 boxes
 where
  gMinus2 = enumerateModelsByTopology GA ++ enumerateModelsByTopology GB
  boxes =
    enumerateModelsByTopology BA
      ++ enumerateModelsByTopology BB
      ++ enumerateModelsByTopology BC
      ++ enumerateModelsByTopology BD
