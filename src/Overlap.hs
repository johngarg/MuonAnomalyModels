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
  noSM  = fromList $ map showField (filter (not . isSM) fields)
  noSM' = fromList $ map showField (filter (not . isSM) fields')

overlap :: [Model] -> [Model] -> [(Model, Model)]
overlap models models' =
  [ (m, m') | m <- models, m' <- models', allSameExotics m m' ]

onlyOneExotic :: Model -> Bool
onlyOneExotic (Model _ fields) =
  length ((toList . fromList) (filter (not . isSM) fields)) == 1
