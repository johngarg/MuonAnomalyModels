-- | This module defines the different irreps that can furnish the topologies.

module TopologyData where

import           Enumerate


-- Irrep data
-- NOTE: All b -> sμμ data is in the order ψ1, φ1, ψ2, φ2
-- NOTE: Topology "c" contains two VLFs

lorentz :: Topo -> [Irrep]
lorentz G1  = map applySU2SU2 [(1, 0), (0, 0), (1, 0)]
lorentz G2  = map applySU2SU2 [(1, 0), (1, 0), (0, 0), (0, 0)]
lorentz B2a = map applySU2SU2 [(1, 0), (1, 0), (0, 0), (1, 0), (1, 0), (0, 0)]
lorentz B2b = map applySU2SU2 [(1, 0), (1, 0), (0, 0), (1, 0), (1, 0), (0, 0)]
lorentz _   = map applySU2SU2 [(1, 0), (0, 0), (1, 0), (0, 0)]

colour :: Topo -> Int -> Int -> [[Irrep]]
colour B1a a b = map
  (map applySU3)
  [ [(a, b), (b, a), (b - 1, a), (a, b)]
  , [(a, b), (b, a), (b, a + 1), (a, b)]
  , [(a, b), (b, a), (b + 1, a - 1), (a, b)]
  ]
colour B1b a b = map
  (map applySU3)
  [ [(a, b), (b, a), (a - 1, b), (b, a)]
  , [(a, b), (b, a), (a, b + 1), (b, a)]
  , [(a, b), (b, a), (a + 1, b - 1), (b, a)]
  ]
colour B2a a b = map
  (map applySU3)
  [ [(a, b), (b, a), (b, a), (a, b), (b, a), (a - 1, b)]
  , [(a, b), (b, a), (b, a), (a, b), (b, a), (a, b + 1)]
  , [(a, b), (b, a), (b, a), (a, b), (b, a), (a + 1, b - 1)]
  ]
colour B2b a b = map
  (map applySU3)
  [ [(a, b), (b, a), (b, a), (b, a - 1), (a - 1, b), (b, a - 1)]
  , [(a, b), (b, a), (b, a), (b + 1, a), (a, b + 1), (b + 1, a)]
  , [(a, b), (b, a), (b, a), (b - 1, a + 1), (a + 1, b - 1), (b - 1, a + 1)]
  ]
colour B3a a b = map
  (map applySU3)
  [ [(a, b), (b, a), (a, b), (b - 1, a)]
  , [(a, b), (b, a), (a, b), (b, a + 1)]
  , [(a, b), (b, a), (a, b), (b + 1, a - 1)]
  ]
colour B3b a b = map
  (map applySU3)
  [ [(a, b), (b, a), (b - 1, a), (b - 1, a)]
  , [(a, b), (b, a), (b, a + 1), (b, a + 1)]
  , [(a, b), (b, a), (b + 1, a - 1), (b + 1, a - 1)]
  ]
colour G1 a b = map (map applySU3) [[(a, b), (b, a), (b, a)]] -- scalar
colour G2 a b = map (map applySU3) [[(a, b), (b, a), (b, a), (b, a), (a, b)]] -- central fermion

isospinB1Template :: Int -> [[Int]]
isospinB1Template i =
  [ [i, i + 1, i, i + 1]
  , [i, i + 1, i + 2, i + 1]
  , [i, i - 1, i, i - 1]
  , [i, i - 1, i - 2, i - 1]
  ]

isospin :: Topo -> Int -> [[Irrep]]
isospin B1a i = map (map SU2) (isospinB1Template i)
isospin B1b i = map (map SU2) (isospinB1Template i)
isospin B2a i = map
  (map SU2)
  [ [i, i, i + 1, i, i, i + 1]
  , [i, i, i + 1, i, i, i - 1]
  , [i, i, i - 1, i, i, i + 1]
  , [i, i, i - 1, i, i, i - 1]
  ]
isospin B2b i = map
  (map SU2)
  [ [i, i, i + 1, i + 2, i + 2, i + 1]
  , [i, i, i + 1, i, i, i + 1]
  , [i, i, i + 1, i, i, i - 1]
  , [i, i, i - 1, i - 2, i - 2, i - 1]
  , [i, i, i - 1, i, i, i - 1]
  , [i, i, i - 1, i, i, i + 1]
  ]
isospin B3a i = map
  (map SU2)
  [ [i, i + 1, i, i + 1]
  , [i, i + 1, i, i - 1]
  , [i, i - 1, i, i + 1]
  , [i, i - 1, i, i - 1]
  ]
isospin B3b i = map
  (map SU2)
  [ [i, i + 1, i + 2, i + 1]
  , [i, i + 1, i, i + 1]
  , [i, i + 1, i, i - 1]
  , [i, i - 1, i - 2, i - 1]
  , [i, i - 1, i, i - 1]
  , [i, i - 1, i, i + 1]
  ]
isospin G1 i = map (map SU2) [[i, i, i + 1], [i, i, i - 1]]
isospin G2 i = map (map SU2) [[i, i, i, i + 1, i + 1], [i, i, i, i - 1, i - 1]]

hypercharge :: Topo -> Int -> [Irrep]
hypercharge B1a y = map U1 [y, 3 - y, 2 - y, y - 3]
hypercharge B1b y = map U1 [y, 3 - y, y - 4, 3 - y]
hypercharge B2a y = map U1 [y, -y, 3 - y, y, -y, y - 1]
hypercharge B2b y = map U1 [y, -y, 3 - y, 4 - y, y - 4, 1 - y]
hypercharge B3a y = map U1 [y, 3 - y, y, -y - 1]
hypercharge B3b y = map U1 [y, 3 - y, 2 - y, -y - 1]

hypercharge G1  y = map U1 [y, -6 - y, -y - 3]
hypercharge G2  y = map U1 [y, -y, -6 - y, -y - 3, y + 3]

-- Central function of module
enumerateModelsByTopology :: Topo -> [Model]
enumerateModelsByTopology topo = filter isCleanModel
  $ fieldCombinations topo lorentz colour isospin hypercharge
