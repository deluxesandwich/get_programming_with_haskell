-- Q17.2
-- If your Events and Probs types were data types and not just synonyms, you
-- could make them instances of Semigroup and Monoid, where combineEvents and
-- combineProbs were the <> operator in each case. Refactor these types and make
-- instances of Semigroup and Monoid.
data Events =
  Events [String]
  deriving (Show)

data Probs =
  Probs [Double]
  deriving (Show)

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where
    combiner = \x y -> mconcat [x, "-", y]

instance Semigroup Events where
  (<>) events1 (Events []) = events1
  (<>) (Events []) events2 = events2
  (<>) events3 events4     = combineEvents events3 events4

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) probs1 (Probs []) = probs1
  (<>) (Probs []) probs2 = probs2
  (<>) probs3 probs4     = combineProbs probs3 probs4

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable =
  PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = e1 <> e2
      newProbs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)
