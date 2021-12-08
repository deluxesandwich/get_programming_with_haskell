ifEven f x =
  if even x
    then f x
    else x

genIfXEven x = (\f -> ifEven f x)

getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> p -> [Char] -> [Char]
genHostRequestBuilder host =
  (\apiKey resource id -> getRequestURL host apiKey resource)

-- exampleUrlBuilder = genHostRequestBuilder "https:\\www.example.com"
exampleUrlBuilder = getRequestURL "https:\\www.example.com"

exampleUrlApiResourceBuilder = exampleUrlBuilder "API_KEY" "RESOURCE_KEY"

-- genApiRequestBuilder hostBuilder apiKey resource =
--   (\id -> hostBuilder apiKey resource id)
subtract2 = flip (-) 2

-- Q5.1
-- Now that you know about partial application,
-- you no longer need to use genIfEvenX.
-- Redefine ifEvenInc, ifEvenDouble, and ifEvenSquare
-- by using ifEven and partial application.
inc x = x + 1

double x = x * 2

square x = x ^ 2

ifEvenInc = ifEven inc

ifEvenDouble = ifEven double

ifEvenSquare = ifEven square

-- Q5.2
-- Even if Haskell didn’t have partial application,
-- you could hack together some approximations.
-- Following a similar pattern to flipBinaryArgs (figure 5.6),
-- write a function binaryPartialApplication that
-- takes a binary function and one argument
-- and returns a new function waiting for the missing
-- argument.
sub x y = x - y

binaryPartialApplication f x = (\y -> f x y)
