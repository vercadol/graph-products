module GraphProducts.LexicographicalSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import GraphProducts
import GraphProducts.Internal
import Data.Graph as Graph
import Data.Set as Set

spec :: Spec
spec = do
    describe "Lexicographical Edges" $ do
        it "works if graph has no vertices" $ do
          lexicographicalEdges (Graph.buildG (0,-1) []) (Graph.buildG (10,11) [(10,11)]) `shouldBe` Set.fromList []
        it "works if one graph is just a single vertex" $ do
          lexicographicalEdges (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) []) `shouldBe` Set.fromList [((0,0),(1,0)),((1,0),(2,0)),((2,0),(0,0))]
          lexicographicalEdges (Graph.buildG (0,0) []) (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) `shouldBe` Set.fromList [((0,0),(0,1)),((0,1),(0,2)),((0,2),(0,0))]
          lexicographicalEdges (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` Set.fromList []
        it "works for simple graphs" $ do
          lexicographicalEdges (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)]) `shouldBe` Set.fromList [((0,0),(0,1)),((0,0),(1,0)),((0,0),(1,1)),((0,1),(1,0)),((0,1),(1,1)),((1,0),(1,1))]

    describe "Lexicographical Product" $ do
        it "works if graph has no vertices" $ do
          lexicographicalGraphProduct (Graph.buildG (0,-1) []) (Graph.buildG (10,11) [(10,11)]) `shouldBe` (Graph.buildG (0,-1) [])
        it "works if one graph is just a single vertex" $ do
          lexicographicalGraphProduct (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
          lexicographicalGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,2) [(0,1), (1,2), (2,0)]) `shouldBe` (Graph.buildG (0,2) [(0,1),(1,2),(2,0)])
          lexicographicalGraphProduct (Graph.buildG (0,0) []) (Graph.buildG (0,0) []) `shouldBe` (Graph.buildG (0,0) [])
        it "works for simple graphs" $ do
          lexicographicalGraphProduct (Graph.buildG (0,1) [(0,1)]) (Graph.buildG (0,1) [(0,1)]) `shouldBe` (Graph.buildG (0,3) [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)])


