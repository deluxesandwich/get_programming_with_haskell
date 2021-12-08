import           Data.Char

add3 x = x + 3

fToAll f []     = []
fToAll f (x:xs) = f x : fToAll f xs

remove test [] = []
remove test (x:xs) =
  if test x
    then remove test xs
    else x : remove test xs

-- example func for tesing myFoldl
myAdd a b = a + b

myProduct []       = 0
myProduct [x]      = x
myProduct [x, y]   = x * y
myProduct (x:y:xs) = x * y * myProduct xs

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where
    rightResult = myFoldr f init xs

-- Q9.1
-- Use filter and length to re-create the elem function.
myElem val seq = length filtered /= 0
  where
    filtered = filter (val ==) seq

-- Q9.2
-- Your isPalindrome function from lesson 6
-- doesn’t handle sentences with spaces or capitals.
-- Use map and filter to make sure the phrase
-- “A man a plan a canal Panama” is recognized as
-- a palindrome.
isPalindrome seq = reverse uppered == uppered
  where
    stripped = filter (' ' /=) seq
    uppered = map toUpper stripped

-- Q9.3
-- In mathematics, the harmonic series is the sum
-- of 1/1 + 1/2 + 1/3 + 1/4 .... Write a function
-- harmonic that takes an argument n and calculates
-- the sum of the series to n.
-- Make sure to use lazy evaluation.
sumOfHarmonic n = sum (take n harmonic)
  where
    harmonic = map (1.0 /) [1.0,2.0 ..]
