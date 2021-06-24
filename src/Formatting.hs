-- | Module for printing Models, Fields and Irreps nicely

module Formatting where

import           Enumerate
import           Data.Ratio

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
formatU1 dontConj (U1 y) = if denomStr == "1"
  then numStr
  else numStr ++ "/" ++ denomStr
 where
  numStr   = show $ numerator rat
  denomStr = show $ denominator rat
  rat      = (y % 6) * (if dontConj then 1 else (-1))
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
formatSM _ _ _ _ = "_"

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
