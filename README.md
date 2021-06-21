# Graph products

Semestral work for a course NI-AFP at FIT CTU. Small haskell library for performing graph products.

## Overview

The graph product functions is performed over `Data.Graph`. Following functions are implemented:

1. Cartesian product

2. Tensor product

3. Lexicographical product

4. Strong product

5. Co-normal product

6. Modular product

For further specification of the types of graph products see the English Wikipedia: https://en.wikipedia.org/wiki/Graph_product

## Dependencies

### Library:
- base >=4.7 && <5 
- containers
### Tests:
- base >=4.7 && <5 
- graph-products
- containers
- hspec
- QuickCheck
### Example with Graphviz
- base >=4.7 && <5
- graph-products
- containers
- graphviz
- text

## How to run it?
A simple example how to use `graph-products` library with Graphviz is prepared. You can take an advantage 
of actually seeing the graphs you created. The example contains two hardcoded graphs and performs 
all types of products, their translation to `.dot` representation and writing them into files.

To run this example, use attached Makefile. It creates a directory `exampleGraphs`, where you 
will find the `.dot` representation of the graphs and their products, as well as their images in `.png` format.

To run the example use your linux terminal:
> make

The library itself is a `stack` project, you can run tests using command
> stack test

To use functions performing graph products in your code, get the library `graph-products` from Hackage and
import GraphProducts in your haskell module:
> import GraphProducts