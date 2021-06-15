{-|
Module      : GraphProducts.Internal
Description : Internal module for the graph product computations, contains helper functions.
Copyright   : (c) Veronika Dolanska, 2021
License     : GPL-3
Maintainer  : dolanver@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Internal module for the graph product computations, contains helper functions that are used in the public module GraphProducts.
-}

module GraphProducts.Internal
    ( cartesianVertices
    , cartesianEdges
    , tensorEdges
    , lexicographicalEdges
    , generalGraphProduct
    ) where

import Data.Graph as Graph
import Data.List as List
import Data.Set as Set

-- | Vertex of resulting graph = tuple: (Vertex of graph1, Vertex of graph2)
type TupleVertex = (Graph.Vertex, Graph.Vertex)

-- | Edge of resulting graph = tuple: (TupleVertex, TupleVertex)
type TupleEdge = (TupleVertex, TupleVertex)

{-|
    Function to compute the vertices of resulting graph.
    The new vertices are created as cartesian product of graph1's vertices and grpah2's vertices.
    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
cartesianVertices :: Graph -> Graph -> Set TupleVertex
cartesianVertices graph1 graph2 =
    Set.cartesianProduct (Set.fromList (vertices graph1)) (Set.fromList (vertices graph2))

{-|
    Function to compute the set of all possible edges in resulting graph, ie the set of all pairs (ordered) of the resulting graph's vertices.
    It takes set of resulting graph's vertices (of type 'Set TupleVertices' ) as a parameter.
-}
allPossibleEdges :: Set TupleVertex -> Set TupleEdge
allPossibleEdges vertices =
    Set.filter (\(a, b) -> a /= b) all
    where all = Set.cartesianProduct vertices vertices

{-|
    Function to determine whether the given edge exists in the given graph.
    It takes graph of type 'Data.Graph' and edge of type '(Data.Graph.Vertex, Data.Graph.Vertex)' as parameters.
-}
hasEdge :: Graph -> (Graph.Vertex, Graph.Vertex) -> Bool
hasEdge graph edge =
    edge `List.elem` (edges graph)

{-|
    Function to filter the edges of resulting graph based on two conditions. First condition is for graph1, second for graph2.
    It takes two functions of type '((Graph.Vertex, Graph.Vertex) -> Bool)' and graph1, graph2 of type 'Data.Graph' as parameters.
-}
getEdgesConditional :: ((Graph.Vertex, Graph.Vertex) -> Bool) -> ((Graph.Vertex, Graph.Vertex) -> Bool) -> Graph -> Graph -> Set TupleEdge
getEdgesConditional condOnGraph1 condOnGraph2 graph1 graph2 =
    Set.filter (\((a1, _), (b1, _)) -> condOnGraph1 (a1, b1))
        . Set.filter (\((_, a2), (_, b2)) -> condOnGraph2 (a2, b2))
        $ allPossibleEdges (cartesianVertices graph1 graph2)

{-|
    Function to filter the edges of a graph that was created by cartesian product.
    Let's define '~' as "there exists an edge between the two vertices in the input graph"
    Condition for (a1, b1) ~ (a2, b2): (a1 == b1 and a2 ~ b2) or (a1 ~ b1 and a2 == b2)
    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
cartesianEdges :: Graph -> Graph -> Set TupleEdge
cartesianEdges graph1 graph2 =
    Set.union firstCondition secondCondition
    where
        firstCondition = getEdgesConditional (\(a1, b1) -> a1 == b1) (hasEdge graph2) graph1 graph2
        secondCondition = getEdgesConditional (hasEdge graph1) (\(a2, b2) -> a2 == b2) graph1 graph2

{-|
    Function to filter the edges of a graph that was created by tensor product.
    Let's define '~' as "there exists an edge between the two vertices in the input graph"
    Condition for (a1, b1) ~ (a2, b2): (a1 ~ b1 and a2 ~ b2)
    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
tensorEdges :: Graph -> Graph -> Set TupleEdge
tensorEdges graph1 graph2 =
    getEdgesConditional (hasEdge graph1) (hasEdge graph2) graph1 graph2

{-|
    Function to filter the edges of a graph that was created by lexicographical product.
    Let's define '~' as "there exists an edge between the two vertices in the input graph"
    Condition for (a1, b1) ~ (a2, b2): (a1 ~ b1) or (a1 == b1 and a2 ~ b2)
    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
lexicographicalEdges :: Graph -> Graph -> Set TupleEdge
lexicographicalEdges graph1 graph2 =
    Set.union firstCondition secondCondition
    where
        firstCondition = getEdgesConditional (hasEdge graph1) (\(a2, b2) -> True) graph1 graph2
        secondCondition = getEdgesConditional (\(a1, b1) -> a1 == b1) (hasEdge graph2) graph1 graph2

{-|
    General (helper) function to create a graph product.
    The resulting graph has vertices of type 'Data.Graph.Vertex' (which is alias to 'Int'). The values of the vertices are
    integers from 0 to (number of vertices - 1).
    It takes a function of type '(Graph -> Graph -> Set TupleEdge)' which returns the edges in resulting graph
    and graph1, graph2 of type 'Data.Graph' as parameters.
-}
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
