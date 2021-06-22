module GraphProducts.ConormalSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import GraphProducts
import GraphProducts.Internal
import Data.Graph as Graph
import Data.Set as Set

spec :: Spec
spec = do
    describe "Co-normal Edges" $ do
        it "works if graph has no vertices" $ do
          conormalEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList []
          conormalEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
          conormalEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
          conormalEdges (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) []) `shouldBe`
            Set.fromList [((0,0),(1,0)), ((1,0),(2,0)), ((2,0),(0,0))]
          conormalEdges (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
        it "works for simple graphs" $ do
          conormalEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)]) `shouldBe`
            Set.fromList [((0,0),(1,0)),((0,1),(1,1)),((0,0),(0,1)),((1,0),(1,1)),((0,0),(1,1)),((0,1),(1,0)),((1,0),(0,1))]
          conormalEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` Set.fromList [((0,0),(0,1)),((0,0),(1,0)),((0,0),(1,1)),((0,0),(1,2)),((0,1),(0,2)),((0,1),(1,0)),
                                     ((0,1),(1,1)),((0,1),(1,2)),((0,2),(0,0)),((0,2),(1,0)),((0,2),(1,1)),((0,2),(1,2)),
                                     ((1,0),(0,1)),((1,0),(1,1)),((1,1),(0,2)),((1,1),(1,2)),((1,2),(0,0)),((1,2),(1,0))]

    describe "Co-normal Product" $ do
        it "works if graph has no vertices" $ do
          conormalGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` (Graph.buildG (0,-1) [])
          conormalGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
          conormalGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
        it "works if one graph is just a single vertex" $ do
          conormalGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
            `shouldBe` (Graph.buildG (0,2) [(0,1), (1,2), (2,0)])
          conormalGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,0) [])
        it "works for simple graphs" $ do
          conormalGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` (Graph.buildG (0,3) [(0,1), (0,2), (1,2), (1,3), (2,1), (2,3), (0,3)])
          conormalGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` (Graph.buildG (0,5) [(0,1),(0,3),(0,4),(0,5),(1,2),(1,3),(1,4),(1,5),(2,0),(2,3),(2,4),(2,5),
                                            (3,1),(3,4),(4,2),(4,5),(5,0),(5,3)])
