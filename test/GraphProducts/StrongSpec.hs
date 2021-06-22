module GraphProducts.StrongSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import GraphProducts
import GraphProducts.Internal
import Data.Graph as Graph
import Data.Set as Set

spec :: Spec
spec = do
    describe "Strong Edges" $ do
        it "works if graph has no vertices" $ do
          strongEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList []
          strongEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
          strongEdges (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
          strongEdges (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
            `shouldBe` Set.fromList [((0,0),(1,0)), ((1,0),(2,0)), ((2,0),(0,0))]
          strongEdges (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
        it "works for simple graphs" $ do
          strongEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` Set.fromList [((0,0),(1,0)),((0,1),(1,1)),((0,0),(0,1)),((1,0),(1,1)),((0,0),(1,1))]
          strongEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
            `shouldBe` Set.fromList [((0,0),(0,1)),((0,1),(0,2)),((0,2),(0,0)),((1,0),(1,1)),((1,1),(1,2)),((1,2),(1,0)),
                                     ((0,0),(1,0)),((0,1),(1,1)),((0,2),(1,2)),((0,1),(1,2)),((0,0),(1,1)),((0,2),(1,0))]


    describe "Strong Product" $ do
        it "works if graph has no vertices" $ do
          strongGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,1) [(0,1)]) `shouldBe` (Graph.buildG (0,-1) [])
          strongGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
          strongGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (0,-1) []) `shouldBe` (Graph.buildG (0,-1) [])
        it "works if one graph is just a single vertex" $ do
          strongGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) [])
            `shouldBe` (Graph.buildG (0,2) [(0,1), (1,2), (2,0)])
          strongGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,0) [])
        it "works for simple graphs" $ do
          strongGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)])
            `shouldBe` (Graph.buildG (0,3) [(0,1), (0,2), (1,3), (2,3), (0,3)])
          strongGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,2) [(0,1),(1,2),(2,0)]) `shouldBe`
            (Graph.buildG (0,5) [(0,1),(1,2),(2,0),(3,4),(4,5),(5,3),(0,3),(1,4),(2,3),(2,5), (1,5),(0,4)])
