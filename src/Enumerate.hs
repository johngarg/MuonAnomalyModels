module Enumerate where

import           Data.List
import           Data.Ratio

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
isClean (SU3    a b) = False
isClean (SU2SU2 0 0) = True
isClean (SU2SU2 1 0) = True
isClean (SU2SU2 0 1) = True
isClean (SU2SU2 a b) = False

isCleanField :: Field -> Bool
isCleanField (Field su2su2 su3 su2 u1) = all isClean [su2su2, su3, su2, u1]

isCleanModel :: Model -> Bool
isCleanModel = all isCleanField

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
  , y   <- [-6 .. 6]
  , col <- colour topo a b
  , iso <- isospin topo i
  ]

isSM :: Field -> Bool
isSM (Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 3)) = True -- H
isSM (Field (SU2SU2 1 0) (SU3 0 0) (SU2 1) (U1 (-3))) = True -- L
isSM (Field (SU2SU2 1 0) (SU3 1 0) (SU2 1) (U1 1)) = True -- Q
isSM (Field (SU2SU2 1 0) (SU3 0 1) (SU2 0) (U1 (-4))) = True -- ub
isSM (Field (SU2SU2 1 0) (SU3 0 1) (SU2 0) (U1 2)) = True -- db
isSM (Field (SU2SU2 1 0) (SU3 0 0) (SU2 0) (U1 6)) = True -- eb
isSM _ = False

-- Bool here is whether or not to return conjugate string for comparison with
-- Python code
formatSU3 :: Bool -> Irrep -> String
formatSU3 _     (SU3 0 0) = "1"
formatSU3 True  (SU3 1 0) = "3"
formatSU3 True  (SU3 0 1) = "3b"
formatSU3 True  (SU3 2 0) = "6"
formatSU3 True  (SU3 0 2) = "6b"
formatSU3 False (SU3 1 0) = "3b"
formatSU3 False (SU3 0 1) = "3"
formatSU3 False (SU3 2 0) = "6b"
formatSU3 False (SU3 0 2) = "6"
formatSU3 _     (SU3 1 1) = "8"
formatSU3 _     (SU3 _ _) = "NotImplemented"
formatSU3 _     _         = "_"

formatSU2 :: Irrep -> String
formatSU2 (SU2 i) = show (i + 1)
formatSU2 _       = "_"

formatU1 :: Bool -> Irrep -> String
formatU1 dontConj (U1 y) = if denom == "1" then num else (num ++ "/" ++ denom)
 where
  num   = show $ numerator rat
  denom = show $ denominator rat
  rat   = (y % 6) * (if dontConj then 1 else (-1))
formatU1 _ _ = "_"

formatSM :: Bool -> Irrep -> Irrep -> Irrep -> String
formatSM dontConj (SU3 a b) (SU2 i) (U1 y) =
  "("
    ++ formatSU3 ((y >= 0) || dontConj) (SU3 a b)
    ++ ", "
    ++ formatSU2 (SU2 i)
    ++ ", "
    ++ formatU1 ((y >= 0) || dontConj) (U1 y)
    ++ ")"

showField :: Field -> String
-- SM fields
showField (Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 3)) = "H"
showField (Field (SU2SU2 1 0) (SU3 0 0) (SU2 1) (U1 (-3))) = "L"
showField (Field (SU2SU2 1 0) (SU3 1 0) (SU2 1) (U1 1)) = "Q"
showField (Field (SU2SU2 1 0) (SU3 0 1) (SU2 0) (U1 (-4))) = "ub"
showField (Field (SU2SU2 1 0) (SU3 0 1) (SU2 0) (U1 2)) = "db"
showField (Field (SU2SU2 1 0) (SU3 0 0) (SU2 0) (U1 6)) = "eb"
showField (Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 (-3))) = "Hd"
showField (Field (SU2SU2 0 1) (SU3 0 0) (SU2 1) (U1 3)) = "Ld"
showField (Field (SU2SU2 0 1) (SU3 0 1) (SU2 1) (U1 (-1))) = "Qd"
showField (Field (SU2SU2 0 1) (SU3 1 0) (SU2 0) (U1 4)) = "ubd"
showField (Field (SU2SU2 0 1) (SU3 1 0) (SU2 0) (U1 (-2))) = "dbd"
showField (Field (SU2SU2 0 1) (SU3 0 0) (SU2 0) (U1 (-6))) = "ebd"

-- Exotic fields
showField (Field (SU2SU2 0 0) su3 su2 u1) = "S" ++ formatSM False su3 su2 u1
showField (Field (SU2SU2 1 0) su3 su2 u1) = "F" ++ formatSM True su3 su2 u1
showField (Field (SU2SU2 _ _) _ _ _) = "NotImplemented"
showField Field{} = "_"
