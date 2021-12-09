import qualified Data.Map as Map

-- Q18.1
-- For the types Triple and Box, implement a function similar to map, tripleMap,
-- and boxMap.
data Box a =
  Box a
  deriving (Show)

boxMap :: (a -> b) -> Box a -> Box b
boxMap mapFunc (Box a) = Box (mapFunc a)

data Triple a =
  Triple a a a
  deriving (Show)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap mapFunc (Triple a b c) = Triple (mapFunc a) (mapFunc b) (mapFunc c)

-- Q18.2
-- Modify the Organ type so that it can be used as a key. Then build a Map,
-- organ-Inventory, of each organ to its count in the organCatalog.
data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Enum, Ord)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCount :: [Int]
organCount = map countOrgan allOrgans
  where
    countOrgan = \organ -> length (filter (organ ==) values)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCount)
