-- data SixSidedDie
--   = S1
--   | S2
--   | S3
--   | S4
--   | S5
--   | S6
-- instance Show SixSidedDie where
--   show S1 = "1"
--   show S2 = "2"
--   show S3 = "3"
--   show S4 = "4"
--   show S5 = "5"
--   show S6 = "6"
-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _   = False
-- Q14.2
-- Define a five-sided die (FiveSidedDie type). Then define a type class named
-- Die and at least one method that would be useful to have for a die. Also
-- include superclasses you think make sense for a die. Finally, make your
-- FiveSidedDie an instance of Die.
data FiveSidedDie
  = S1
  | S2
  | S3
  | S4
  | S5
  deriving (Enum, Eq, Show)

class (Enum a, Eq a) =>
      Die a
  where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)

newFiveSidedDie :: FiveSidedDie
newFiveSidedDie = S1
-- *Main> (roll 5) :: FiveSidedDie
-- S1
-- *Main> (roll 2) :: FiveSidedDie
-- S3
-- *Main> (roll 3) :: FiveSidedDie
-- S4
