import           Control.Exception              ( evaluate )
import           Test.Hspec
import           Test.QuickCheck

import           Enumerate
import           Formatting
import           Overlap
import           TopologyData

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    -- it "returns the first element of a list" $ do
    --   head [23 ..] `shouldBe` (23 :: Int)
    -- it "returns the first element of an *arbitrary* list" $ property $ \x xs ->
    --   head (x : xs) == (x :: Int)
    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
    it "checks that field is a SM one" $ do
      isSM h && isSM q && isSM hc
    it "checks that all exotics in list are the same" $ do
      allSameExotics m1 m2 && allSameExotics m2 m1 && not (allSameExotics m1 m3)
    it "checks overlapping exotic field content" $ do
      overlap [m1, m3] [m2, m4] `shouldBe` [(m1, m2)]
    it "checks overlapping exotic field content accounting for conj" $ do
      overlap [m1, m3] [m1', m4] `shouldBe` [(m1, m1')]
    it "checks S3 model in B1a completions" $ do
      overlap [Model G1 [s3]] (enumerateModelsByTopology B1a)
        `shouldBe` [(Model G1 [s3], Model B1a [l, s3, q, s3])]
 where
  m1  = Model G1 [(Field (SU2SU2 0 0) (SU3 0 0) (SU2 5) (U1 3)), h]
  m1' = Model G1 [(Field (SU2SU2 0 0) (SU3 0 0) (SU2 5) (U1 (-3)))]
  m2  = Model B3a [(Field (SU2SU2 0 0) (SU3 0 0) (SU2 5) (U1 3)), q]
  m3  = Model B1a [(Field (SU2SU2 0 0) (SU3 0 0) (SU2 4) (U1 1)), q]
  m4  = Model
    B3b
    [ (Field (SU2SU2 0 0) (SU3 0 0) (SU2 5) (U1 3))
    , (Field (SU2SU2 0 0) (SU3 1 1) (SU2 5) (U1 3))
    , q
    ]
  h  = Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 3)
  hc = Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 (-3))
  q  = Field (SU2SU2 1 0) (SU3 1 0) (SU2 1) (U1 1)
  l  = Field (SU2SU2 1 0) (SU3 0 0) (SU2 1) (U1 (-3))
  s3 = Field (SU2SU2 0 0) (SU3 0 1) (SU2 2) (U1 2)
