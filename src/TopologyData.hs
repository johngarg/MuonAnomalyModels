-- | This module defines the different irreps that can furnish the topologies.

module TopologyData where

import           Enumerate


-- Irrep data
-- NOTE: All b -> sμμ data is in the order ψ1, φ1, ψ2, φ2
-- NOTE: Topology "c" contains two VLFs

lorentz :: Topo -> [Irrep]
lorentz BC = map applySU2SU2 [(1, 0), (1, 0), (0, 0), (1, 0), (1, 0), (0, 0)]
lorentz _  = map applySU2SU2 [(1, 0), (0, 0), (1, 0), (0, 0)]

colour :: Topo -> Int -> Int -> [[Irrep]]
colour BA a b = map
  (map applySU3)
  [ [(a, b), (b - 1, a), (a, b - 1), (b - 1, a)]
  , [(a, b), (b + 1, a - 1), (a - 1, b + 1), (b + 1, a - 1)]
  , [(a, b), (b, a + 1), (a + 1, b), (b, a + 1)]
  ]
colour BB a b = map
  (map applySU3)
  [ [(a, b), (b - 1, a), (a, b), (b, a)]
  , [(a, b), (b + 1, a - 1), (a, b), (b, a)]
  , [(a, b), (b, a + 1), (a, b), (b, a)]
  ]
colour BC a b = map
  (map applySU3)
  [ [(a, b), (b, a), (b, a), (b, a - 1), (a - 1, b), (a - 1, b)]
  , [(a, b), (b, a), (b, a), (b - 1, a + 1), (a + 1, b - 1), (a + 1, b - 1)]
  , [(a, b), (b, a), (b, a), (b + 1, a), (a, b + 1), (a, b + 1)]
  ]
colour BD a b = map
  (map applySU3)
  [ [(a, b), (b, a), (b - 1, a), (b - 1, a)]
  , [(a, b), (b, a), (b + 1, a - 1), (b + 1, a - 1)]
  , [(a, b), (b, a), (b, a + 1), (b, a + 1)]
  ]
colour GA a b = map (map applySU3) [[(a, b), (b, a), (a, b)]] -- scalar
colour GB a b = map (map applySU3) [[(a, b), (b, a), (b, a), (a, b), (b, a)]] -- central fermion

isospin :: Topo -> Int -> [[Irrep]]
isospin BC i = map
  (map SU2)
  [ [i, i, i + 1, i + 2, i + 2, i + 1]
  , [i, i, i + 1, i, i, i + 1]
  , [i, i, i + 1, i, i, i - 1]
  , [i, i, i - 1, i - 2, i - 2, i - 1]
  , [i, i, i - 1, i, i, i - 1]
  , [i, i, i - 1, i, i, i + 1]
  ]
isospin GA i = map (map SU2) [[i, i, i + 1], [i, i, i - 1]]
isospin GB i = map (map SU2) [[i, i, i, i + 1, i + 1], [i, i, i, i - 1, i - 1]]
isospin _  i = map
  (map SU2)
  [ [i, i + 1, i + 2, i + 1]
  , [i, i + 1, i, i + 1]
  , [i, i + 1, i, i - 1]
  , [i, i - 1, i - 2, i - 1]
  , [i, i - 1, i, i - 1]
  , [i, i - 1, i, i + 1]
  ]

hypercharge :: Topo -> Int -> [Irrep]
hypercharge BA y = map U1 [y, -y - 1, y + 4, -y - 1]
hypercharge BB y = map U1 [y, -y - 1, y, 3 - y]
hypercharge BC y = map U1 [y, -y, 3 - y, 4 - y, y - 4, y - 1]
hypercharge BD y = map U1 [y, 3 - y, 2 - y, -y - 1]
hypercharge GA y = map U1 [y, -6 - y, y + 3]
hypercharge GB y = map U1 [y, -y, -6 - y, y + 3, -y - 3]
