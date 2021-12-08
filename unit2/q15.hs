data FourLetterAlphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize letter = toEnum rotation
  where
    halfAlphabetSize = alphabetSize `div` 2
    offset = fromEnum letter + halfAlphabetSize
    rotation = offset `mod` alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else 1 + fromEnum c + halfN
    rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotCharDecoder = rotNdecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (v1 /= v2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    padLength = maxBits - length reversedBits
    leadingFalses = take padLength (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    size = length bits
    indices = [size - 1,size - 2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =
  map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plaintextBits)
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar encodedBits
  where
    encodedBits = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot =
  Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad =
  OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 (fromEnum (maxBound :: Char))

makePrngPadInt :: (Int -> Int) -> Int -> [Int]
makePrngPadInt prngGen seed = newSeed : makePrngPadInt prngGen newSeed
  where
    newSeed = prngGen seed

makePrngPad :: (Int -> Int) -> Int -> String
makePrngPad prngGen seed = map bitsToChar prngPadBits
  where
    prngPadInt = makePrngPadInt prngGen seed
    prngPadBits = map intToBits prngPadInt

type Seed = Int

data StreamCipher =
  SC Int Int Seed

instance Cipher StreamCipher where
  encode (SC a b seed) text = applyOTP prngPad text
    where
      prngGen = prng a b (fromEnum (maxBound :: Char))
      prngPad = makePrngPad prngGen seed
  decode (SC a b seed) text = applyOTP prngPad text
    where
      prngGen = prng a b (fromEnum (maxBound :: Char))
      prngPad = makePrngPad prngGen seed

instance Show StreamCipher where
  show (SC a b seed) =
    "StreamCipher a:" ++ show a ++ " b:" ++ show b ++ " seed:" ++ show seed

myStremCipher :: StreamCipher
myStremCipher = SC 10 20 7000
