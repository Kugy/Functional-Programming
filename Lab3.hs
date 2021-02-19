-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab3(Fruit(Apple,Banana,Lemon),sumPrice,BSTree(Void,BSNode),subTree,Tree(Node),count,labels,height,(++),elem,last,reverse,filter) where

import Prelude hiding ((++),elem,last,reverse,filter)

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1 -}
{-
    Fruit is a dataype with 3 constructors Apple, Banana and Lemon.
    Apple and Banana take values of type Double as argument and Lemon takes a
    value of type Integer.

    INVARIANT: The arguments that all the Fruit constructors take cannot be negative.
-}
data Fruit = Apple Double | Banana Double | Lemon Integer deriving (Show)

{- 1.2 -}
{-
    sumPrice
    Takes a list of Fruit and calculates the price of each fruit either by 
    weight or by unit and returns the sum of all prices.
    RETURNS: Sum of the prices of all the Fruit in a list of Fruit
    EXAMPLE: sumPrice [Banana 4.0, Apple 3.0, Lemon 7] 3.0 2.0 5.0 = 52
-}
--VARIANT: length of f
sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
sumPrice [] _ _ _ = 0
sumPrice [Apple x] a _ _ = x * a
sumPrice [Banana y] _ b _ = y * b
sumPrice [Lemon z] _ _ c = (fromInteger z) * c
sumPrice f a b c = (sumPrice (tail f) a b c) + (sumPrice [head f] a b c)

{- 2 -}

{- Binary search trees

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree  -- do not modify this line

{-
    subTree a b (BSNode l x r)
    
    Takes a 2 Integer values and a BSTree as argument and returns a BSTree
    with the values that are >= and < than the 2 integer values respectively
    
    RETURNS: A BSTree where each value in the BSTree are greater than or equal to
    and less than the 2 Integer values passed in as argument.
    
    EXAMPLES: subTree 5 8 t (where t is a BSTree containing nodes with values ranging from 5-9) = (Void 5 Void) 6 (Void 7 Void)
-}
--VARIANT: length of BSNode l x r
subTree :: Integer -> Integer -> BSTree -> BSTree
subTree a b Void = Void

subTree a b (BSNode Void x Void)
    | x >= a && x < b = BSNode Void x Void
    | otherwise = Void

subTree a b (BSNode Void x r)
    | x >= a && x < b = BSNode Void x (subTree a b r)
    | otherwise = (subTree a b r)

subTree a b (BSNode l x r)
    | x >=a && x < b = BSNode (subTree a b l) x (subTree a b r)
    | x >=a && x >= b = (subTree a b l)
    | otherwise = (subTree a b r)

{- 3.1 -}
{-
    Node a [Tree a] represents a tree with root a and child of that
    node which is a list [Tree a]

    INVARIANT: The Tree contains at least 1 node
-}
data Tree a = Node a [Tree a] deriving (Show)

{- 3.2 a) -}
{-
    count Node l (r:rs)
    Counts the number of nodes in a Tree
    RETURNS: An Integer value after counting the number of nodes in a Tree
    EXAMPLE: count (Node "hej" []) = 1
-}
--VARIANT: Node l (r:rs)
count:: Tree a -> Integer
count (Node a []) = 1
count (Node l (r:rs)) = count r + count (Node l rs)

{- 3.2 b) -}
{-
    labels (Node a (r:rs))
    
    Takes a Tree as argument and returns a list of all the values 
    of the nodes in the Tree

    RETURNS: A list of all the node values in a Tree

    EXAMPLE: labels (Node "hej" []) = ["hej"]
-}
--VARIANT: Node a (r:rs)
labels:: Tree a -> [a]
labels (Node a []) = [a]
labels (Node a (r:rs)) = labels r ++ labels(Node a rs)

{- 3.2 c) -}
{-
    height (Node a (r:rs))
    
    Takes a Tree as argument and calculates the height of the Tree

    RETURNS: Returns an Integer value which is the height of the given Tree

    EXAMPLE: labels (Node 1 [Node 2 []]) = 2 
-}
--VARIANT: Node a (r:rs)
height:: Tree a -> Integer
height (Node a []) = 1
height (Node a (r:rs)) = 1 + maximum (map height (r:rs))

{- 4.1 -}
{-
    (++) x y
    
    Takes to lists as arguments and prepends the first list to the second
    RETURNS: Returns a list where data from list x is prepended to the list y
    EXAMPLE: (++) [1,2,3] [4,5] = [1,2,3,4,5] 
-}
(++) :: [a] -> [a] -> [a]
(++) x y = foldr (:) y x

{- 4.2 -}
{-
    elem a b
    
    Takes any value and a list of same type as the value and 
    checks to see if the value exists in the list
    RETURNS: Returns True or False if the b contains a or not.
    EXAMPLE: elem 1 [1,2] = True
-}
elem :: Eq a => a -> [a] -> Bool
elem a b = foldl (\xs x -> if x == a then True else xs) False b

{- 4.3 -}
{-
    last a
    
    Takes a list as argument and returns the last value in the list
    
    PRE: List cannot be empty
    
    RETURNS: Returns the last value in list a.
    
    EXAMPLE: last ['h','a','s','k','e','l','l'] = 'l'
-}
last :: [a] -> a
last a = foldl (\lastValue x -> x) (error "List is empty") a

{- 4.4 -}
{-
    reverse b
    
    Takes a list as argument and returns a list containing the values from that list
    but in reverse order
    
    RETURNS: Returns a list containing all values from b but in reverse order.
    
    EXAMPLE: reverse [1,2,3,4,5] = [5,4,3,2,1]
-}
reverse :: [a] -> [a]
reverse b = foldl (\xs x -> x:xs) [] b

{- 4.5 -}
{-
    filter f b
    
    Takes a function (of return type Bool) and list as arguments and returns a list
    of values from the list provided, where the function applies to each value in the list.
    
    RETURNS: Returns a list of values from b if the value returned from f is True.
 
    EXAMPLE: filter even [1,2,3,4] = [2,4]
-}
filter :: (a->Bool) -> [a] -> [a]
filter f b = foldr (\x xs -> if f x then x : xs else xs) [] b
