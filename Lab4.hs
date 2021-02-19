-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab4(dot,connectedComponent) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

-- import your Graph module
import Graph(Graph,empty,addVertex,addEdge,vertices,neighbors)
import Data.List ((\\))

{- 3 -}
{-
    dot graph
-- It is a function that takes a graph as argument which returns string representation of the graph
--RETURNS: It computes a string representation of the graph in the DOT graph description language.
Inspired with the usage of "\\" from    http://www.cantab.net/users/antoni.diller/haskell/units/unit02.html
EXAMPLE from Test case: g1 = Graph.addVertex (Graph.addVertex (Graph.addVertex Graph.empty "a") "b") "c" computes
gives the string representation in Dot language as 
                        dot g1 :
                        graph {
                                c;
                                b;
                                a;
                            };
-}
dot :: Graph String -> String
vertex :: [[Char]] -> [Char]
vertex [] = ""
vertex (x:vertices) = "\n  "++x++";"++(vertex vertices)
edges :: Eq b => Graph b -> [(b, b)]
edges graph = do {x <- (Graph.vertices graph); y <- (Graph.neighbors graph x); return (x,y)}
dEdges :: Eq b => [(b, b)] -> [(b, b)]
dEdges list = dEdges list []
    where   dEdges [] matching = matching
            dEdges ((x,y):lst) matching |((x,y)  `elem` ((x,y):lst)) ==False= dEdges (((x,y):lst) \\ ((x,y):matching)) ((x,y):matching)
                                        |((y,x) `elem`((x,y):lst)) ==True= dEdges (((x,y):lst) \\ ((x,y):matching)) ((x,y):matching)
                                |otherwise = dEdges lst matching
dot graph = "graph {"++(vertex (Graph.vertices graph))++unwords ["\n "++(fst x)++" -- "++(snd x)++";" | x <- (dEdges ( edges graph))]++"\n}"

{- 4 -}
{- connectedComponent graph1 vertex

connectedComponent is a function given with a graph g and a vertex v of it, returns the connected components of the v from the given graph g.
RETURNS : Returns the edges connected to the vertex v from the graph g.
EXAMPLE : From testcase for graph2 
            g2 = Graph.addEdge (Graph.addEdge (Graph.addEdge g1 ("a","b")) ("a","c")) ("b","c")
             connectedComponent g2 "a" == Graph ["a","b","c"] [("a","b"),("a","c"),("b","c")]
-}
connectedComponent :: Eq a => Graph a -> a -> Graph a
connectedComponent graph1 vertex = depthfirst [vertex] (Graph.addVertex Graph.empty vertex) [] vertex
    where   depthfirst list graph newstate current
                | and [node `elem` newstate | node <- list] = graph
                | otherwise = depthfirst (Graph.vertices (dfrecursive graph (Graph.neighbors graph1 current) current)) 
                (dfrecursive graph (Graph.neighbors graph1 current) current) (current:newstate) (head (list \\ newstate))
            dfrecursive newgraph [] _ = newgraph
            dfrecursive newgraph (x:lst) current = dfrecursive (Graph.addEdge (Graph.addVertex newgraph x) (current,x)) lst current
