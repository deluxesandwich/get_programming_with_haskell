import qualified Distribution.SPDX as Q8

testSeq = [1 .. 10]

myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n - 1) xs

myDrop 0 seq    = seq
myDrop _ []     = []
myDrop n (x:xs) = myDrop (n - 1) xs

-- 1.    Identify the end goal(s).
-- 2.    Determine what happens when a goal is reached.
-- 3.    List all alternate possibilities.
-- 4.    Determine your “rinse and repeat” process.
-- 5.    Ensure that each alternative moves you toward the goal.
-- 1.   empty list
-- 2.   empty list length is 0
-- 3.   non-empty list
-- 4.   decrement until empty, accumulate count
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- myCycle (first:rest) = first:myCycle (rest++[first])
myCycle seq = seq ++ myCycle seq

-- Q8.1
-- Implement your own version of reverse, which reverses
-- a list.
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Q8.2
fastFib _ _ 0     = 0
fastFib _ _ 1     = 1
fastFib _ _ 2     = 1
fastFib x y 3     = x + y
fastFib x y count = fastFib (x + y) x (count - 1)
