type FirstName = String

type LastName = String

type MiddleName = String

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist

data Author =
  Author Name

data Artist
  = Person Name
  | Band String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book =
  Book
    { author    :: Creator
    , isbn      :: String
    , bookTitle :: String
    , bookYear  :: Int
    , bookPrice :: Double
    }

data VinylRecord =
  VinylRecord
    { artist      :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
    }

data CollectibleToy =
  CollectibleToy
    { name       :: String
    , descrption :: String
    , toyPrice   :: Double
    }

-- Q16.1
-- To further complicate the items in your store, you eventually keep an inventory
-- of free pamphlets. Pamphlets have a title, a description, and a contact field
-- for the organization that provides the pamphlet. Create the Pamphlet type and
-- add it to StoreItem. Additionally, modify the price so that it works with
-- Pamphlet.
data Pamphlet =
  Pamphlet
    { title         :: String
    , description   :: String
    , contact       :: String
    , pamphletPrice :: Double
    }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book)         = bookPrice book
price (RecordItem record)     = recordPrice record
price (ToyItem toy)           = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

-- Q16.2
-- Create a Shape type that includes the following shapes: Circle, Square, and
-- Rectangle. Then write a function to compute the perimeter of a Shape as well
-- as its area.
type Radius = Double

type Length = Double

type Width = Double

data Shape
  = Circle Radius
  | Square Length
  | Rectangle Length Width

getPerimeter :: Shape -> Double
getPerimeter (Circle radius)          = radius * 2 * pi
getPerimeter (Square length)          = length * 4
getPerimeter (Rectangle length width) = length * 2 + width + 2

getArea :: Shape -> Double
getArea (Circle radius)          = pi * radius ^ 2
getArea (Square length)          = length ^ 2
getArea (Rectangle length width) = length * width
