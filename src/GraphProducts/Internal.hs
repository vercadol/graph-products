module GraphProducts.Internal
    ( cartesianVertices
    , cartesianEdges
    , cartesianGraphProduct
    , tensorEdges
    , tensorGraphProduct
    , lexicographicalEdges
    , lexicographicalGraphProduct
    ) where

import Data.Graph as Graph
import Data.List as List
import Data.Set as Set

type TupleVertex = (Graph.Vertex, Graph.Vertex)
type TupleEdge = (TupleVertex, TupleVertex)


cartesianVertices :: Graph -> Graph -> Set TupleVertex
cartesianVertices graph1 graph2 =
    Set.cartesianProduct (Set.fromList (vertices graph1)) (Set.fromList (vertices graph2))

cartesianEdges :: Graph -> Graph -> Set TupleEdge
cartesianEdges graph1 graph2 =
    Set.fromList ((expandEdgesToVertices (edges graph1) (vertices graph2)) ++ switchTupleElements (expandEdgesToVertices (edges graph2) (vertices graph1)))
    where
        expandEdgesToVertices [] vertices = []
        expandEdgesToVertices ((v1, v2):edges) vertices = (List.map (\vertice -> ((v1, vertice),(v2, vertice))) vertices) ++ expandEdgesToVertices edges vertices
        switchTupleElements = List.map (\((a, b), (c, d)) -> ((b, a), (d, c)))

tensorEdges :: Graph -> Graph -> Set TupleEdge
tensorEdges graph1 graph2 =
    Set.fromList (expandEdgesToEdges (edges graph1) (edges graph2))
    where
        expandEdgesToEdges [] _ = []
        expandEdgesToEdges _ [] = []
        expandEdgesToEdges ((v1, v2):edges1) edges2 = (List.map (\(v3, v4) -> ((v1,v3),(v2,v4))) edges2) ++ expandEdgesToEdges edges1 edges2

lexicographicalEdges :: Graph -> Graph -> Set TupleEdge
lexicographicalEdges graph1 graph2 =
    Set.fromList (firstCondition ++ secondCondition)
    where
        firstCondition = switchTupleElements (expandEdgesToVertices (edges graph2) (vertices graph1))
        secondCondition = expandEdgesToEdges (edges graph1) (allTuples (vertices graph2))
        expandEdgesToVertices [] vertices = []
        expandEdgesToVertices ((v1, v2):edges) vertices = (List.map (\vertice -> ((v1, vertice),(v2, vertice))) vertices) ++ expandEdgesToVertices edges vertices
        switchTupleElements = List.map (\((a, b), (c, d)) -> ((b, a), (d, c)))

        expandEdgesToEdges [] _ = []
        expandEdgesToEdges _ [] = []
        expandEdgesToEdges ((v1, v2):edges1) edges2 = (List.map (\(v3, v4) -> ((v1,v3),(v2,v4))) edges2) ++ expandEdgesToEdges edges1 edges2
        allTuples lst = Set.toList (Set.cartesianProduct (Set.fromList lst) (Set.fromList lst))



generalGraphProduct :: (Graph -> Graph -> Set TupleEdge) -> Graph -> Graph -> Graph
generalGraphProduct edgesFunction graph1 graph2 =
    case mappedEdges of
        Just edges -> Graph.buildG (0, nrVertices - 1) edges
        _ -> error "Error occured in creating GraphProduct"
    where
        vertices = Set.toList (cartesianVertices graph1 graph2)
        edges = Set.toList (edgesFunction graph1 graph2)
        nrVertices = List.length vertices
        mapEdge (v1, v2) = case (List.elemIndex v1 vertices, List.elemIndex v2 vertices) of
                            (Just mappedV1, Just mappedV2) -> Just (mappedV1, mappedV2)
                            _ -> Nothing
        mappedEdges = mapM mapEdge edges

cartesianGraphProduct :: Graph -> Graph -> Graph
cartesianGraphProduct = generalGraphProduct cartesianEdges

tensorGraphProduct :: Graph -> Graph -> Graph
tensorGraphProduct = generalGraphProduct tensorEdges

lexicographicalGraphProduct :: Graph -> Graph -> Graph
lexicographicalGraphProduct = generalGraphProduct lexicographicalEdges

