-- Types
-- :t x in GHCI to get type of x
-- :t works for functions as well 
-- -> useful when we don't exactly know what type declaration we should give a function

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

-- Type variables
-- whenever a function can take multiple types, they are declared with type variables
head' :: [a] -> a
head' xs = xs !! 0

-- Typeclasses
showThree = show 3
readThree = read "3" :: Int -- explicit type annotation needed here

smallestInt = minBound :: Int
biggestInt = maxBound :: Int

lengthOfListPlusOneHalf = fromIntegral (length [1,2,3]) + 0.5

