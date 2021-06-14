module Lib
    ( someFunc
    , cartesianVertices
    , cartesianEdges
    , cartesianGraphProduct
    , tensorEdges
    , tensorGraphProduct
    ) where

import Data.Graph as Graph
import Data.List as List
import Data.Set as Set

someFunc :: IO ()
someFunc = putStrLn "someFunc"

cartesianVertices :: Graph -> Graph -> Set (Graph.Vertex, Graph.Vertex)
cartesianVertices graph1 graph2 =
    Set.cartesianProduct (Set.fromList (vertices graph1)) (Set.fromList (vertices graph2))

cartesianEdges :: Graph -> Graph -> Set ((Graph.Vertex, Graph.Vertex), (Graph.Vertex, Graph.Vertex))
cartesianEdges graph1 graph2 =
    Set.fromList ((expandEdgesToVertices (edges graph1) (vertices graph2)) ++ switchTupleElements (expandEdgesToVertices (edges graph2) (vertices graph1)))
    where
        expandEdgesToVertices [] vertices = []
        expandEdgesToVertices ((v1, v2):edges) vertices = (List.map (\vertice -> ((v1, vertice),(v2, vertice))) vertices) ++ expandEdgesToVertices edges vertices
        switchTupleElements = List.map (\((a, b), (c, d)) -> ((b, a), (d, c)))

cartesianGraphProduct :: Graph -> Graph -> Graph
cartesianGraphProduct graph1 graph2 =
    case mappedEdges of
        Just edges -> Graph.buildG (0, nrVertices - 1) edges
        _ -> error "Error occured in cartesianGraphProduct"
    where
        vertices = Set.toList (cartesianVertices graph1 graph2)
        edges = Set.toList (cartesianEdges graph1 graph2)
        nrVertices = List.length vertices
        mapEdge (v1, v2) = case (List.elemIndex v1 vertices, List.elemIndex v2 vertices) of
                            (Just mappedV1, Just mappedV2) -> Just (mappedV1, mappedV2)
                            _ -> Nothing
        mappedEdges = mapM mapEdge edges

tensorEdges :: Graph -> Graph -> Set ((Graph.Vertex, Graph.Vertex), (Graph.Vertex, Graph.Vertex))
tensorEdges graph1 graph2 =
    Set.fromList (expandEdgesToEdges (edges graph1) (edges graph2))
    where
        expandEdgesToEdges [] _ = []
        expandEdgesToEdges _ [] = []
        expandEdgesToEdges ((v1, v2):edges1) edges2 = (List.map (\(v3, v4) -> ((v1,v3),(v2,v4))) edges2) ++ expandEdgesToEdges edges1 edges2

tensorGraphProduct :: Graph -> Graph -> Graph
tensorGraphProduct graph1 graph2 =
    case mappedEdges of
        Just edges -> Graph.buildG (0, nrVertices - 1) edges
        _ -> error "Error occured in cartesianGraphProduct"
    where
        vertices = Set.toList (cartesianVertices graph1 graph2)
        edges = Set.toList (tensorEdges graph1 graph2)
        nrVertices = List.length vertices
        mapEdge (v1, v2) = case (List.elemIndex v1 vertices, List.elemIndex v2 vertices) of
                            (Just mappedV1, Just mappedV2) -> Just (mappedV1, mappedV2)
                            _ -> Nothing
        mappedEdges = mapM mapEdge edges