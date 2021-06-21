import Test.Hspec

import qualified GraphProducts.CartesianSpec
import qualified GraphProducts.TensorSpec
import qualified GraphProducts.LexicographicalSpec
import qualified GraphProducts.StrongSpec
import qualified GraphProducts.ConormalSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CartesianGraphProduct"   GraphProducts.CartesianSpec.spec
  describe "TensorGraphProduct"  GraphProducts.TensorSpec.spec
  describe "LexicographicalGraphProduct"  GraphProducts.LexicographicalSpec.spec
  describe "StrongGraphProduct"  GraphProducts.StrongSpec.spec
  describe "ConormalGraphProduct"  GraphProducts.ConormalSpec.spec
