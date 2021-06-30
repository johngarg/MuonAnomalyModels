-- | This module provides the core functionality to enumerate the fields that
-- enter the topologies specified here

module Enumerate where

import           Data.List

-- Data model
data Irrep = SU2 Int | SU3 Int Int | SU2SU2 Int Int | U1 Int deriving (Show, Eq)
data Field = Field Irrep Irrep Irrep Irrep deriving Show
data Topo = BA | BB | BC | BD | GA | GB -- *B*ox (or *B* anomaly) and *G* minus 2 diagrams

type Model = [Field]

-- Functions that need to be defined to enumerate combinations
-- lorentz :: Topo -> [Irrep]
-- colour :: Topo -> Int -> Int -> [[Irrep]]
-- isospin :: Topo -> Int -> [[Irrep]]
-- hypercharge :: Topo -> Int -> [Irrep]

-- Functions
applyIrrep :: (Int -> Int -> Irrep) -> (Int, Int) -> Irrep
applyIrrep g (a, b) = g a b

applySU3 :: (Int, Int) -> Irrep
applySU3 = applyIrrep SU3

applySU2SU2 :: (Int, Int) -> Irrep
applySU2SU2 = applyIrrep SU2SU2

-- Need to clean irreps so there are no negative dynkins
isClean :: Irrep -> Bool
isClean (U1  _     ) = True
isClean (SU2 a     ) = (a >= 0) && (a < 4)
isClean (SU3    0 0) = True
isClean (SU3    1 0) = True
isClean (SU3    0 1) = True
isClean (SU3    2 0) = True
isClean (SU3    0 2) = True
isClean (SU3    _ _) = False
isClean (SU2SU2 0 0) = True
isClean (SU2SU2 1 0) = True
isClean (SU2SU2 0 1) = True
isClean (SU2SU2 _ _) = False

isCleanField :: Field -> Bool
isCleanField (Field su2su2 su3 su2 u1) = all isClean [su2su2, su3, su2, u1]

isCleanModel :: Model -> Bool
isCleanModel = all isCleanField

isSM :: Field -> Bool
isSM (Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 3)) = True    -- H
isSM (Field (SU2SU2 1 0) (SU3 0 0) (SU2 1) (U1 (-3))) = True -- L
isSM (Field (SU2SU2 1 0) (SU3 1 0) (SU2 1) (U1 1)) = True    -- Q
isSM (Field (SU2SU2 1 0) (SU3 0 1) (SU2 0) (U1 (-4))) = True -- ub
isSM (Field (SU2SU2 1 0) (SU3 0 1) (SU2 0) (U1 2)) = True    -- db
isSM (Field (SU2SU2 1 0) (SU3 0 0) (SU2 0) (U1 6)) = True    -- eb
isSM _ = False

fieldCombinations
  :: Topo
  -> (Topo -> [Irrep])
  -> (Topo -> Int -> Int -> [[Irrep]])
  -> (Topo -> Int -> [[Irrep]])
  -> (Topo -> Int -> [Irrep])
  -> [Model]
fieldCombinations topo lorentz colour isospin hypercharge =
  [ zipWith4 Field (lorentz topo) col iso (hypercharge topo y)
  | a   <- [0 .. 3]
  , b   <- [0 .. 3]
  , i   <- [0 .. 3]
  , y   <- [-12 .. 12]
  , col <- colour topo a b
  , iso <- isospin topo i
  ]  -- Multiple choices for colour and isospin so include in comprehension
