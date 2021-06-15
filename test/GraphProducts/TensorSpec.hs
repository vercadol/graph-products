module GraphProducts.TensorSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import GraphProducts.Internal
import Data.Graph as Graph
import Data.Set as Set

spec :: Spec
spec = do
    describe "Tensor Edges" $ do
        it "works if graph has no vertices" $ do
          tensorEdges (Graph.buildG (0,-1) []) (Graph.buildG (10,11) [(10,11)]) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
          tensorEdges (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
          tensorEdges (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
        it "works for simple graphs" $ do
          tensorEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList [((0,0),(1,1))]

    describe "Tensor Product" $ do
        it "works if graph has no vertices" $ do
          tensorGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (10,11) [(10,11)]) `shouldBe` (Graph.buildG (0,-1) [])
        it "works if one graph is just a single vertex" $ do
          tensorGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,2) [])
          tensorGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,0) [])
        it "works for simple graphs" $ do
          tensorGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)]) `shouldBe` (Graph.buildG (0,3) [(0,3)])
          tensorGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,2) [(0,1), (1,2), (2,0)])
            `shouldBe` (Graph.buildG (0,8) [(0,4),(1,5),(2,3),(3,7),(4,8),(5,6),(6,1),(7,2),(8,0)])


