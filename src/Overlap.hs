-- | Functions to check for overlap between the g-2 and B-anomaly models

module Overlap where

import           Enumerate
import           Data.Set                       ( fromList )

-- Check subsets
allSameExotics :: Model -> Model -> Bool
allSameExotics (Model _ fields) (Model _ fields') = noSM == noSM'
 where
  noSM  = fromList (filter isSM fields)
  noSM' = fromList (filter isSM fields')

overlap :: [Model] -> [Model] -> [(Model, Model)]
overlap models models' =
  [ (m, m') | m <- models, m' <- models', allSameExotics m m' ]
