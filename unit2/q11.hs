import qualified Distribution.SPDX as Q11

halve :: Integer -> Integer
halve x = x `div` 2

printDouble :: Integer -> String
printDouble x = show (x * 2)

makeAddress :: Integer -> String -> String -> (Integer, String, String)
makeAddress number street town = (number, street, town)

addressNum = 102

addressStreet = "1st St."

addressTown = "Shanghai"

makeAddressNumber :: String -> String -> (Integer, String, String)
makeAddressNumber = makeAddress addressNum

-- Q11.1
-- What is the type signature for filter? How is it different from map?
-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter returns a Bool instead
--
-- Q11.2
-- In Haskell, both tail and head have an error when called on an empty list.
-- You can write a version of tail that won’t fail but instead return an empty
-- list when called on an empty list. Can you write a version of head that returns
-- an empty list when called on an empty list? To answer this, start by writing
-- out the type signatures of both head and tail.
-- head :: [a] -> a
myTail :: [a] -> [a]
myTail []     = []
myTail (x:xs) = xs

-- f :: (a -> b -> a)
-- f is a binary function that takes a, b and returns a
-- What’s the type signature of this function below? Note: foldl has a different type signature.
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where
    newInit = f init x
