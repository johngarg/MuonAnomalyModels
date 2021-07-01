module Main where

import           Enumerate
import           Formatting
import           TopologyData
import           Overlap

-- Central function of module
enumerateModelsByTopology :: Topo -> [Model]
enumerateModelsByTopology topo = filter isCleanModel
  $ fieldCombinations topo lorentz colour isospin hypercharge

-- Read in tree-level models from Python output
readModels :: IO [[String]]
readModels = do
  let fileName = "data/treelevelmodels.csv"
  contents <- readFile fileName
  let models = map (read :: String -> [String]) (lines contents)
  return models

main :: IO ()
main = print (enumerateModelsByTopology BB)
