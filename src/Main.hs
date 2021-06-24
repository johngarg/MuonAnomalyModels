module Main where

import           Enumerate
import           Formatting
import           TopologyData

-- Central function of module
enumerateModelsByTopology :: Topo -> [Model]
enumerateModelsByTopology topo = filter isCleanModel
  $ fieldCombinations topo lorentz colour isospin hypercharge

main :: IO ()
main = print $ map showField $ head (enumerateModelsByTopology BC)
