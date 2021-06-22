module GraphProducts.ModularSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import GraphProducts
import GraphProducts.Internal
import Data.Graph as Graph
import Data.Set as Set

spec :: Spec
spec = do
    describe "Modular Edges" $ do
        it "works if graph has no vertices" $ do
          modularEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList []
          modularEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
          modularEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
          modularEdges (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
          modularEdges (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
        it "works for simple graphs" $ do
          modularEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` Set.fromList [((0,0),(1,1)),((1,1),(0,0))]
          modularEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` Set.fromList [((0,0),(1,1)),((1,1),(0,0)),((0,1),(1,2)),((1,2),(0,1)),((0,2),(1,0)),((1,0),(0,2))]

    describe "Modular Product" $ do
        it "works if graph has no vertices" $ do
          modularGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` (Graph.buildG (0,-1) [])
          modularGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
          modularGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
        it "works if one graph is just a single vertex" $ do
          modularGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
            `shouldBe` (Graph.buildG (0,2) [])
          modularGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,0) [])
        it "works for simple graphs" $ do
          modularGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` (Graph.buildG (0,3) [(0,3), (3,0)])
          modularGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` (Graph.buildG (0,5) [(0,4),(4,0),(1,5),(5,1),(2,3),(3,2)])
