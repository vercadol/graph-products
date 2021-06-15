import Test.Hspec

import qualified GraphProducts.CartesianSpec
import qualified GraphProducts.TensorSpec
import qualified GraphProducts.LexicographicalSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CartesianGraphProduct"   GraphProducts.CartesianSpec.spec
  describe "TensorGraphProduct"  GraphProducts.TensorSpec.spec
  describe "LexicographicalGraphProduct"  GraphProducts.LexicographicalSpec.spec
