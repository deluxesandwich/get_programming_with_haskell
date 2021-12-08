data IceCream
  = Vanilla
  | Chocolate
  deriving (Ord, Eq, Show)

-- Q1:
-- See which flavor Haskell thinks is superior by deriving the Ord type class.
fav :: IceCream -> IceCream -> IceCream
fav iceCreamA iceCreamB =
  if iceCreamA >= iceCreamB
    then iceCreamA
    else iceCreamB

-- ghci> fav Vanilla Chocolate
-- Chocolate
--
-- Q13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
    else succ n
