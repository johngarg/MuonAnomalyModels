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
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    it "returns the first element of an *arbitrary* list" $ property $ \x xs ->
      head (x : xs) == (x :: Int)
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
    it "checks that field is a SM one" $ do
      isSM h && isSM q
    it "checks that all exotics in list are the same" $ do
      allSameExotics m1 m2
 where
  m1 = Model GA [(Field (SU2SU2 0 0) (SU3 0 0) (SU2 5) (U1 3)), h]
  m2 = Model BB [(Field (SU2SU2 0 0) (SU3 0 0) (SU2 5) (U1 3)), q]
  h  = (Field (SU2SU2 0 0) (SU3 0 0) (SU2 1) (U1 3))
  q  = (Field (SU2SU2 1 0) (SU3 1 0) (SU2 1) (U1 1))
