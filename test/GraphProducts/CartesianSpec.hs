module GraphProducts.CartesianSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import GraphProducts
import GraphProducts.Internal
import Data.Graph as Graph
import Data.Set as Set

spec :: Spec
spec = do
    describe "Cartesian vertices" $ do
        it "works if graph has no vertices" $ do
          cartesianVertices (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList []
          cartesianVertices (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
          cartesianVertices (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
            cartesianVertices (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
                `shouldBe` Set.fromList [(0, 0), (1, 0), (2, 0)]
            cartesianVertices (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList [(0,0)]
        it "works for simple graphs" $ do
          cartesianVertices (Graph.buildG (0,2) [(0,1),(1,2)]) (Graph.buildG (10,11) [(10,11)])
            `shouldBe` Set.fromList [(0, 10), (1, 10), (2, 10), (0, 11), (1, 11), (2, 11)]
          cartesianVertices (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` Set.fromList [(0,0),(1,0),(0,1),(1,1)]
          cartesianVertices (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` Set.fromList [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

    describe "Cartesian Edges" $ do
        it "works if graph has no vertices" $ do
          cartesianEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList []
          cartesianEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
          cartesianEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
          cartesianEdges (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
            `shouldBe` Set.fromList [((0,0),(1,0)), ((1,0),(2,0)), ((2,0),(0,0))]
          cartesianEdges (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
        it "works for simple graphs" $ do
          cartesianEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` Set.fromList [((0,0),(1,0)),((0,1),(1,1)),((0,0),(0,1)),((1,0),(1,1))]
          cartesianEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` Set.fromList [((0,0),(0,1)),((0,1),(0,2)),((0,2),(0,0)),((1,0),(1,1)),((1,1),(1,2)),((1,2),(1,0)),
                                     ((0,0),(1,0)),((0,1),(1,1)),((0,2),(1,2))]

    describe "Cartesian Product" $ do
        it "works if graph has no vertices" $ do
          cartesianGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` (Graph.buildG (0,-1) [])
          cartesianGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
        it "works if one graph is just a single vertex" $ do
          cartesianGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
            `shouldBe` (Graph.buildG (0,2) [(0,1), (1,2), (2,0)])
          cartesianGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,0) [])
        it "works for simple graphs" $ do
          cartesianGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` (Graph.buildG (0,3) [(0,1), (0,2), (1,3), (2,3)])
          cartesianGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` (Graph.buildG (0,5) [(0,1),(1,2),(2,0),(3,4),(4,5),(5,3),(0,3),(1,4),(2,5)])
