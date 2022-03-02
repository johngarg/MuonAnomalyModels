-- | Functions to check for overlap between the g-2 and B-anomaly models

module Overlap where

import           Enumerate
import           Formatting
import           Data.Set                       ( toList
                                                , fromList
                                                , isSubsetOf
                                                )

-- Check subsets

filterSM :: [Field] -> [Field]
filterSM = filter (not . isSM)

allSameExotics :: Model -> Model -> Bool
allSameExotics (Model _ fields) (Model _ fields') = noSM == noSM'
 where
  noSM  = fromList $ map showField $ filterSM fields
  noSM' = fromList $ map showField $ filterSM fields'

subsetExotics :: Model -> Model -> Bool
subsetExotics (Model _ fields) (Model _ fields') = noSM `isSubsetOf` noSM'
 where
  noSM  = fromList $ map showField $ filterSM fields
  noSM' = fromList $ map showField $ filterSM fields'

overlap :: [Model] -> [Model] -> [(Model, Model)]
overlap models models' =
  [ (m, m') | m <- models, m' <- models', allSameExotics m m' ]

subsetOverlap :: [Model] -> [Model] -> [(Model, Model)]
subsetOverlap models models' =
  [ (m, m') | m <- models, m' <- models', subsetExotics m m' ]

allSameExoticsStr :: [String] -> [String] -> Bool
allSameExoticsStr model model' = m == m'
 where
  m  = fromList $ filter (\x -> length x > 3) model
  m' = fromList $ filter (\x -> length x > 3) model'

overlapStr :: [[String]] -> [[String]] -> [([String], [String])]
overlapStr models models' =
  [ (m, m') | m <- models, m' <- models', allSameExoticsStr m m' ]

nExotics :: Int -> Model -> Bool
nExotics n (Model _ fields) =
  length ((toList . fromList) (filter (not . isSM) fields)) == n


isFermion :: Field -> Bool
isFermion (Field (SU2SU2 1 0) _ _ _) = True
isFermion (Field (SU2SU2 0 1) _ _ _) = True
isFermion _                          = False

areAllFermions :: [Field] -> Bool
areAllFermions fields = all isFermion fields

keepFermionModels :: [Model] -> [Model]
keepFermionModels models = filter (areAllFermions . modelFields) models
