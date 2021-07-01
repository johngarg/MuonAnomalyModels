module Main where

import           Enumerate
import           Formatting
import           TopologyData
import           Overlap

-- Read in tree-level models from Python output
readModels :: String -> IO [[String]]
readModels file = do
  let fileName = file
  contents <- readFile fileName
  let models = map (read :: String -> [String]) (lines contents)
  return models

checkTreeLevelOverlap :: IO ()
checkTreeLevelOverlap = do
  trees  <- readModels "data/treelevelmodels.csv"
  trees' <- readModels "data/treelevelmodels_diracpartners.csv"
  -- Get overlap
  print $ map snd $ overlapStr gMinus2 (trees ++ trees')
 where
  gMinus2 =
    map showModel $ enumerateModelsByTopology GA ++ enumerateModelsByTopology GB


main :: IO ()
main = putStrLn $ tabulateModels $ map snd $ overlap gMinus2 boxes
 where
  gMinus2 = enumerateModelsByTopology GA ++ enumerateModelsByTopology GB
  boxes =
    enumerateModelsByTopology BA
      ++ enumerateModelsByTopology BB
      ++ enumerateModelsByTopology BC
      ++ enumerateModelsByTopology BD
