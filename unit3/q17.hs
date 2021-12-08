data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Transparent
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) Transparent any = any
  (<>) any Transparent = any
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

-- Q17.1
-- Your current implementation of Color doesn’t contain an identity element.
-- Modify the code in this unit so that Color does have an identity element,
-- and then make Color an instance of Monoid.
instance Monoid Color where
  mempty = Transparent
  mappend = (<>)

-- Q17.2
-- If your Events and Probs types were data types and not just synonyms, you
-- could make them instances of Semigroup and Monoid, where combineEvents and
-- combineProbs were the <> operator in each case. Refactor these types and make
-- instances of Semigroup and Monoid.
type Events = [String]

type Probs = [Double]

data PTable =
  PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where
    combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)
