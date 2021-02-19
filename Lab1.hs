-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab1(minus,fun1,fun2,fun3,fun4,fun5,fun6,sumSquareDiff) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1

       product 2
   --> ...if 2 ==1 then 1  else 2* product (2-1)
   .if False then 2* product(2-1)
   .2* product(2-1)
   .2* product (2-1)
   .2* product 1
   .2* if 1 ==1 then 1 else 1* product (1-1)
   .2* if TRUE then 1 else 1* product (1-1)
   .2* 1
   .2

   1.2

   product n
   ...{ product n
	PRE:  n >= 1
	RETURNS: 1*2*3*.....*n
	EXAMPLES: f 0==0, f 2==1
      } 
     product :: Int -> Int
     product 0= 0
     product n= n ∗ product ( n−1)
   1.3

   VARIANT: ...n
	product 1 = 1
	product n= n ∗ product ( n−1)
  -}

{- 2.1 -}
-- remember to provide a function specification
{-
    minus = \inte1 inte2 -> inte1 - inte2
    Subtracts the value of the second Integer from the first Integer, that the user provides as function arguments
    PRE: Both function arguments have to be of type Integer
    RETURNS: Subtractted value of inte2 from inte1
    EXAMPLES: minus 5 4 = 1
              minus 3 6 = -3
-}
minus :: Integer->Integer->Integer
minus = \inte1 inte2 -> inte1 - inte2

{- 2.2
foo is an integer and when evaluated we get an integer value.
   2.3
bar is an Int->Int and when evaluated we get an error message unless and until an Int value is applied to it.
   2.4

       minus 5 4
   --> ...minus 5 4 = 5 - 4
   .minus 5 4 = 1
   .1
 -}

{- 3.1 -}
{-
    fun1 int
    Takes an Integer value as argument and multiplies it with (-1)
    RETURNS: -int
    EXAMPLES: fun1 -1 = 1
              fun1 33 = -33
-}
fun1 :: Integer->Integer
fun1 int = int * (-1)

{- 3.2 -}
{-
    fun2 int1 int2
    Divides the first integer value by the second
    PRE: int2 > 0
    RETURNS: int1/int2
    EXAMPLES: fun2 4 2 = 2
              fun2 33 5 = 6
-}
fun2 :: Integer->Integer->Integer
fun2 int1 int2 = (int1) `div` (int2)

{- 3.3 -}
{-
    fun3 int
    *********
    Multiplies the integer by 2 and 3 in a tuple
    *********
    PRE: int2 > 0
    RETURNS: (int*2, int*3)
    EXAMPLES: fun3 4 = (8,12)
              fun2 0 = (0,0)
-}
fun3 :: Integer->(Integer, Integer)
fun3 int = (int*2, int*3)

{- 3.4 -}
{-
    fun4 (int1, int2)
    Adds the values in the tuples together
    RETURNS: int1 + int2
    EXAMPLES: fun4 (4,4) = 8
              fun4 (3,-4) = -1
-}
fun4 :: (Integer,Integer)->Integer
fun4 (int1,int2) = int1 + int2

{- 3.5 -}
{-
    fun5 int dbl str
    **********
    Converts the Integer and Double arguments into Strings then concatenates them with the String argument
    **********
    RETURNS: "Some text..." ++ show int ++ "Some text..." ++ show dbl ++ "Some text..." ++ "Some text..." ++ str ++ "Some text..."
    EXAMPLES: fun5 3 -12.0 Nafi = "You entered the following: '3' as Integer, '-12.0' as Double and 'Nafi' as String"
-}
fun5 :: Integer->Double->String->String
fun5 int dbl str =
    "You entered the following: '" ++
    show int ++ "' as Integer, '"    ++
    show dbl ++ "' as Double "       ++
    "and '" ++ str ++ "' as String"

{- 3.6 -}
{-
    fun6 int1 str1 str2 int2
    *********
    Multiplies the 2 Integer values together and sets them as the first index of a tuple and concatenates
    the 2 String arguments together and sets them as the second value of the tuple
    *********
    RETURNS: ((int1*int2),(str1+" "+str2))
    EXAMPLES: fun6 3 "Hello" "World" 5 = (15, "Hello World")
-}
fun6 :: Integer->String->String->Integer->(Integer,String)
fun6 int1 str1 str2 int2 = ((int1*int2),(str1++" "++str2))

{- 4 -}
{-
    sumOfSquares num
    Computes the sum of the squares from 1 to num
    PRE: num > 0
    RETURNS: 1^2 + 2^2 + ... num^2
    EXAMPLES: sumOfSquares 3 = 14
-}
-- VARIANT: num
sumOfSquares :: Integer->Integer
sumOfSquares num =
    if num <= 1
    then 1
    else
    (num^2) + sumOfSquares (num-1)

{-
    sumOfNumbers num
    Computes the sum of 1 to num
    PRE: num > 0
    RETURNS: 1 + 2 + 3 +.... num
    EXAMPLES: sumOfNumbers 5 = 15
-}
--VARIANT: num
sumOfNumbers::Integer->Integer
sumOfNumbers num =
    if num <= 1
    then 1
    else
    num + sumOfNumbers(num-1)

{-
    squareOfSumOfNumbers num
    Squares the result obtained from sumOfNumbers function
    PRE: n > 0
    RETURNS: (1 + 2 + 3 +...n)^2
    EXAMPLES: squareOfSumOfNumbers 5 = 225 
-}
squareOfSumOfNumbers :: Integer->Integer
squareOfSumOfNumbers num = (sumOfNumbers num) ^ 2

{-
  sumSquareDiff num
  Computes the difference between square of the sum of 1 to num and the sum of the all the squares from 1 to num
  RETURNS: (1+...num)^2 - (1^2+...n^2)
  EXAMPLES: sumSquareDiff 5 = 170
            sumSquareDiff 13 = 7462 
-}
sumSquareDiff :: Integer->Integer
sumSquareDiff num = (squareOfSumOfNumbers num) - (sumOfSquares num)
