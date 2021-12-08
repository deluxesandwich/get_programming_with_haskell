-- Q6.1
-- Haskell has a function called repeat that takes a value
-- and repeats it infinitely. Using the functions you’ve
-- learned so far, implement your own version of repeat.
myRepeat x = cycle [x]

-- Q6.2
-- Write a function subseq that takes three arguments: a start
-- position, an end position, and a list. The function should
-- return the subsequence between the start and end. For example:
-- GHCi> subseq 2 5 [1 .. 10]
-- [3,4,5]
-- GHCi> subseq 2 7 "a puppy"
-- "puppy"
subseq start end seq = take count (drop start seq)
  where
    count = end - start + 1

-- Q6.3
-- Write a function inFirstHalf that returns True if an element
-- is in the first half of a list, and otherwise returns False.
testSeq = [1 .. 10]

inFirstHalf x seq = x `elem` take halfPoint seq
  where
    halfPoint = length seq `div` 2
