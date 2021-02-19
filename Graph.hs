
module Graph(Graph,empty,addVertex,addEdge,vertices,neighbors) where

-- remember to provide the datatype representation
-- reference link : https://stackoverflow.com/questions/9732084/how-do-you-represent-a-graph-in-haskell 
data Graph a = Graph [a] [(a,a)]

  
{- 1.1 -} 
{- empty
    --- Here it constructs an empty graph with empty vertices and edges.
RETURNS: It empties a graph with a empty list
EXAMPLE: empty = ([],[])
-}
empty :: Graph a
empty = Graph [] []    

{- 1.2 -}
{- addVertex (vertex,edge) x
-- It is an abstract representation of vertices.
-- reference :https://hackage.haskell.org/package/gloss-examples-1.9.4.1/src/picture/Graph/Main.hs
-- PRE:Here checks if a vertex is present already
-- RETURNS:It returns with a vertex for the graph
    EXAMPLE: addVertex ([],[]) x = ([x],[])
-}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph vertex edge ) x-- remove "undefined" and write your function here
    |elem x vertex == True =  (Graph vertex edge)
    |otherwise = (Graph (x:vertex) edge)

{- 1.3 -}
{-  addEdge (vertex,edge)(x,y)

-- remember to provide a function specification
-- PRE:1)Checks for conditions to add an edge.
--     2)Initially checks the presence of vertex so that it can add an edge to it.
--     3)Then it checks if already an edge is present .
-- RETURNS: If its not present, then it adds one list of edges.
 EXAMPLE: addEdge(([a,b],[])) c = ([a,b],[c])
-}
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (Graph vertex edge) (x,y)
    | elem vertex[] == True = error"No vertices are present"
    | (elem (x,y) edge == False && elem (y,x) edge ==False) =(Graph vertex ((x,y):edge)) 
    | otherwise = (Graph vertex edge)
    
{- 1.4 -}
{- vertices(vertex,edge) 
Takes the graph in a polymorphic type.
    -- RETURNS: all the vertices of a graph.
EXAMPLE: vertices ([a,b],[]) = [a,b]
    -}
vertices :: Eq a => Graph a -> [a]
vertices (Graph vertex edge) = vertex

{- 1.5 -}
{-
 neighbors ((vertex,(e):edge)) node

-- This function is to find the neighboring vertices of a given vertex
-- RETURNS:The edges of a graph are taken as input and checks for the edges of vertex "n" to find its neighbors.

-}
neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph _ [])_ = []
neighbors (Graph [] _)_ = []
neighbors (Graph vertex (e:edge)) node
    | node == fst e = snd e : neighbors (Graph vertex edge) node
    | node == snd e = fst e : neighbors (Graph vertex edge) node
    | e :edge == [] = []
    | otherwise = neighbors (Graph vertex edge) node

{- 2 -}
-- Worst time complexity:
-- Time complexity of empty is O(1).
-- Time complexity of addVertex is O(vertex) where vertex is the number of vertices to be added to the graph 
-- Time complexity of addEdge is O(edge) where edge is the number of edges to be added to the graph.
-- TIme complexity of vertices is O(1) as we display just the list of vertices in the graph.
-- TIme complexity of neighbors is O(edge) where edge is the total number of edges present in the graph.  
