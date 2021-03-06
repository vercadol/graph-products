{-|
Module      : GraphProducts
Description : Module for the graph product computations.
Copyright   : (c) Veronika Dolanska, 2021
License     : GPL-3
Maintainer  : dolanskaverca@gmail.com
Stability   : experimental
Portability : POSIX

Module containing functions for the graph product computations. Following types of graph products are implemented: cartesian product,
tensor product, lexicographical product, strong product, co-normal product, modular product.
-}

module GraphProducts
    ( cartesianGraphProduct
    , tensorGraphProduct
    , lexicographicalGraphProduct
    , strongGraphProduct
    , conormalGraphProduct
    , modularGraphProduct
    ) where

import GraphProducts.Internal
import Data.Graph as Graph

{-|
    Function to create a cartesian product of two graphs.
    Vertices of the product are created as cartesian product of the graph1's vertices and graph2's vertices.

    > Let's define '~' as "there exists an edge between the two vertices in the input graph"
    > Condition for (a1, b1) ~ (a2, b2): (a1 == b1 and a2 ~ b2) or (a1 ~ b1 and a2 == b2)

    Vertices of the product are integers from @0@ to @(number of vertices - 1)@. They correspond to the lexicographical order
    of tuples created by cartesian product of the graph1's vertices and graph2's vertices.

    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
cartesianGraphProduct :: Graph -> Graph -> Graph
cartesianGraphProduct = generalGraphProduct cartesianEdges

{-|
    Function to create a cartesian product of two graphs.
    Vertices of the product are created as cartesian product of the graph1's vertices and graph2's vertices.

    > Let's define '~' as "there exists an edge between the two vertices in the input graph"
    > Condition for (a1, b1) ~ (a2, b2): (a1 ~ b1 and a2 ~ b2)

    Vertices of the product are integers from @0@ to @(number of vertices - 1)@. They correspond to the lexicographical order
    of tuples created by cartesian product of the graph1's vertices and graph2's vertices.

    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
tensorGraphProduct :: Graph -> Graph -> Graph
tensorGraphProduct = generalGraphProduct tensorEdges

{-|
    Function to create a cartesian product of two graphs.
    Vertices of the product are created as cartesian product of the graph1's vertices and graph2's vertices.

    > Let's define '~' as "there exists an edge between the two vertices in the input graph"
    > Condition for (a1, b1) ~ (a2, b2): (a1 ~ b1) or (a1 == b1 and a2 ~ b2)

    Vertices of the product are integers from @0@ to @(number of vertices - 1)@. They correspond to the lexicographical order
    of tuples created by cartesian product of the graph1's vertices and graph2's vertices.

    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
lexicographicalGraphProduct :: Graph -> Graph -> Graph
lexicographicalGraphProduct = generalGraphProduct lexicographicalEdges

{-|
    Function to create a strong (normal, AND) product of two graphs.
    Vertices of the product are created as cartesian product of the graph1's vertices and graph2's vertices.

    > Let's define '~' as "there exists an edge between the two vertices in the input graph"
    > Condition for (a1, b1) ~ (a2, b2): (a1 == b1 and a2 ~ b2) or (a1 ~ b1 and a2 == b2) or (a1 ~ b1 and a2 ~ b2)

    Vertices of the product are integers from @0@ to @(number of vertices - 1)@. They correspond to the lexicographical order
    of tuples created by cartesian product of the graph1's vertices and graph2's vertices.

    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
strongGraphProduct :: Graph -> Graph -> Graph
strongGraphProduct = generalGraphProduct strongEdges

{-|
    Function to create a co-normal (disjunctive, OR) product of two graphs.
    Vertices of the product are created as cartesian product of the graph1's vertices and graph2's vertices.

    > Let's define '~' as "there exists an edge between the two vertices in the input graph"
    > Condition for (a1, b1) ~ (a2, b2): (a1 ~ b1) or (a2 ~ b2)

    Vertices of the product are integers from @0@ to @(number of vertices - 1)@. They correspond to the lexicographical order
    of tuples created by cartesian product of the graph1's vertices and graph2's vertices.

    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
conormalGraphProduct :: Graph -> Graph -> Graph
conormalGraphProduct = generalGraphProduct conormalEdges

{-|
    Function to create a modular product of two graphs.
    Vertices of the product are created as cartesian product of the graph1's vertices and graph2's vertices.

    > Let's define '~' as "there exists an edge between the two vertices in the input graph"
    > and '!~' as "there does not exist an edge between the two vertices in the input graph"
    > Condition for (a1, b1) ~ (a2, b2): (a1 != b1 and a2 != b2) and ((a1 ~ b1 and a2 ~ b2) or (a1 !~ b1 and a2 !~ b2))

    Vertices of the product are integers from @0@ to @(number of vertices - 1)@. They correspond to the lexicographical order
    of tuples created by cartesian product of the graph1's vertices and graph2's vertices.

    It takes graph1 and graph2 of type 'Data.Graph' as parameters.
-}
modularGraphProduct :: Graph -> Graph -> Graph
modularGraphProduct = generalGraphProduct modularEdges