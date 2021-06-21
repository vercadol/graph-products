module Graphviz where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Graph as Graph
import Data.List as List

import GraphProducts

toDotGraph :: Graph -> G.DotGraph Graph.Vertex
toDotGraph gr =
    G.graphElemsToDot G.nonClusteredParams (verticesAndLabels (vertices gr)) (edgesAndLabels (edges gr))
    where verticesAndLabels = List.map (\x -> (x, ""))
          edgesAndLabels = List.map (\(v1, v2) -> (v1, v2, ""))

toFile :: String -> G.DotGraph Graph.Vertex -> IO ()
toFile filename dotGraph =
    TL.writeFile filename dotText
    where dotText = G.printDotGraph dotGraph :: TL.Text


main :: IO ()
main = do
  toFile "exampleGraphs/graph1.dot" (toDotGraph graph1)
  toFile "exampleGraphs/graph2.dot" (toDotGraph graph2)
  toFile "exampleGraphs/cartesian.dot" (toDotGraph (cartesianGraphProduct graph1 graph2))
  toFile "exampleGraphs/tensor.dot" (toDotGraph (tensorGraphProduct graph1 graph2))
  toFile "exampleGraphs/lexicographical.dot" (toDotGraph (lexicographicalGraphProduct graph1 graph2))
  where
    graph1 = Graph.buildG (0,1) [(0,1)]
    graph2 = Graph.buildG (0,2) [(0,1), (1,2), (2,0)]
