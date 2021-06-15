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

allPossibleEdges :: Set TupleVertex -> Set TupleEdge
allPossibleEdges vertices =
    Set.filter (\(a, b) -> a /= b) all
    where all = Set.cartesianProduct vertices vertices

hasEdge :: Graph -> (Graph.Vertex, Graph.Vertex) -> Bool
hasEdge graph edge =
    edge `List.elem` (edges graph)

getEdgesConditional :: ((Graph.Vertex, Graph.Vertex) -> Bool) -> ((Graph.Vertex, Graph.Vertex) -> Bool) -> Graph -> Graph -> Set TupleEdge
getEdgesConditional condOnGraph1 condOnGraph2 graph1 graph2 =
    Set.filter (\((a1, _), (b1, _)) -> condOnGraph1 (a1, b1))
        . Set.filter (\((_, a2), (_, b2)) -> condOnGraph2 (a2, b2))
        $ allPossibleEdges (cartesianVertices graph1 graph2)

cartesianEdges :: Graph -> Graph -> Set TupleEdge
cartesianEdges graph1 graph2 =
    Set.union firstCondition secondCondition
    where
        firstCondition = getEdgesConditional (\(a1, b1) -> a1 == b1) (hasEdge graph2) graph1 graph2
        secondCondition = getEdgesConditional (hasEdge graph1) (\(a2, b2) -> a2 == b2) graph1 graph2

tensorEdges :: Graph -> Graph -> Set TupleEdge
tensorEdges graph1 graph2 =
    getEdgesConditional (hasEdge graph1) (hasEdge graph2) graph1 graph2

lexicographicalEdges :: Graph -> Graph -> Set TupleEdge
lexicographicalEdges graph1 graph2 =
    Set.union firstCondition secondCondition
    where
        firstCondition = getEdgesConditional (hasEdge graph1) (\(a2, b2) -> True) graph1 graph2
        secondCondition = getEdgesConditional (\(a1, b1) -> a1 == b1) (hasEdge graph2) graph1 graph2

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

