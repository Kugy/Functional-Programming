-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab2(iota,inter,interOrdered,isMatch) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1

    1. head :: [a] -> a
    2. tail :: [a] -> [a]
    3. \x -> x :: p -> p
    4. (,) :: a -> b -> (a, b)
    5. (:) :: a -> [a] -> [a]
    6. [[]] :: [[a]]
    7. tail [[]] :: [[a]]
    8. id : []  :: [a -> a]
    9. id id :: a -> a
    10. head [id] "foo" :: [Char]

    The polymorphic expressions are (1 -> 9)
-}

{- 2 -}
{-
    iota n
    Takes an Integer value as input and returns a list containing
    the numbers between 0 to n-1 (including 0 and n-1)
    PRE: n >= 0
    RETURNS: List of all the numbers between 0 and n-1 (including 0 and n-1)
    EXAMPLES: iota 5 = [1,2,3,4]
                iota 9 = [1,2,3,4,5,6,7,8]
                iota 0 = []
-}
iota::Integer->[Integer]
iota n = [0..(n-1)]

{- 3.1 -}
{-
    inter (s1:s1s) (s2:s2s)
    Takes 2 lists of Integers as arguments and returns a list of all Integers 
    that appear in both lists (s1:s1s) and (s2:s2s)
    RETURNS: A list of Integer values that appear in both lists (s1:s1s) and (s2:s2s)
    EXAMPLES: inter [1,2,3] [3,4,5] = [3]
                inter [2,3,10,-1] [-1,-2,0,10] = [10,-1]
-}
--VARIANT = length of (s1:s1s) + length of (s2:s2s)
inter :: [Integer]->[Integer]->[Integer]
inter [] (s2:s2s) = []
inter (s1:s1s) [] = []
inter [] [] = []
inter (s1:s1s) (s2:s2s) = 
    if head (s1:s1s) `elem` (s2:s2s) 
    then [s1] ++ inter s1s (s2:s2s) 
    else inter s1s (s2:s2s)

{- 3.2 -}
{-
    interOrdered s1 s2
    Takes 2 lists of Integers as arguments (s1, s2) and returns an ordered
    list of all Integers that appear in both lists
    PRE: Lists provided as arguments have to be ordered lists
    RETURNS: Oredered list of Integer values that appear in both lists s1 and s2
    EXAMPLES: inter [1,2,3] [3,4,5] = [3]
                inter [2,3,-1,10] [-2,-1,0,10] = [-1,10]
-}
interOrdered :: [Integer]->[Integer]->[Integer]
interOrdered s1 s2 = [x | x <- s1, x `elem` s2]

{- 3.3 -}

s1 = iota 100000
--(0.01 secs, 7,253,920 bytes)
s2 = iota 1000000
--(0.21 secs, 72,054,608 bytes)
t1 = inter s1 s2
--(62.79 secs, 59,253,320 bytes)
t2 = interOrdered s1 s2
--(57.15 secs, 12,053,688 bytes)

{- 4 -}
{-
    isMatch s p
    Takes 2 Strings as arguments and checks wheather pattern (p) matches the entire string (s).
    Special characters include '?' which matches any single character and '*' which matches any
    sequence of characters including the empty sequence.
    RETURNS: A Boolean result. True if p matches entire String s otherwise, it returns False
    EXAMPLES: "aa" "?a" = True
                "adceb" "*a*b" = True
                "abs" "?x?" = False
-}
--VARIANT: length of s + length of p
isMatch :: String->String->Bool
isMatch [] [] = True
isMatch (s:ss) [] = False
isMatch [] (p:ps) = False
isMatch _ "*" = True
isMatch s p =
    if (head p) == '*' && (head (tail p)) == (head s)
    then isMatch (tail s) (tail (tail p))
    else if (head p) == '*' && (head (tail p)) /= (head s)
    then isMatch (tail s) p
    else if (head p) == '?' || (head s) == (head p)
    then isMatch (tail s) (tail p)
    else False