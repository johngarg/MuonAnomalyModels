-- | Functions to check for overlap between the g-2 and B-anomaly models

module Overlap where

import           Enumerate
import           Formatting
import           Data.Set                       ( toList
                                                , fromList
                                                )

-- Check subsets
allSameExotics :: Model -> Model -> Bool
allSameExotics (Model _ fields) (Model _ fields') = noSM == noSM'
 where
  noSM  = fromList $ map showField $ filter (not . isSM) fields
  noSM' = fromList $ map showField $ filter (not . isSM) fields'

overlap :: [Model] -> [Model] -> [(Model, Model)]
overlap models models' =
  [ (m, m') | m <- models, m' <- models', allSameExotics m m' ]

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
