-- 1) Curried functions

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- with partially applied function
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

-- also works with infix functions using sections ()
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

-- sections don't work for (-4) as this means minus four 
-- not a function that subtracts four. For this we do:
subtractFour :: (Num a) => a -> a
subtractFour = subtract 4

-- 2) Higher order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- These two have the same result for f x y
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g -- returns function that can be applied to x y
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x -- returns value directly

-- 3) Maps & Filters
-- recursive map function
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- recursive filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- map function using list comprehension
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

-- filter function using list comprehension
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = [x | x <- xs, f x]

-- quicksort using filter
quicksort' :: (Num a, Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerList = filter (<=x) xs
      biggerList = filter (>x) xs
  in quicksort' smallerList ++ [x] ++ quicksort' biggerList

largestNumUnderXdivisibleByY :: (Integral a) => a -> a -> a
largestNumUnderXdivisibleByY x y = head (filter p [x-1,x-2..])
  where p x = x `mod` y == 0

-- takeWhile function goes through list until predicate is false. For example:
firstWord :: String -> String
firstWord = takeWhile (/=' ')

-- sum of all odd squares that are smaller than x
sumSquares :: (Integral a) => a -> a
sumSquares x = sum (takeWhile (<x) (filter odd (map (^2) [1..])))
-- as list comprehension
sumSquares' :: (Integral a) => a -> a
sumSquares' x = sum (takeWhile (<x) [k^2 | k <- [1..], odd (k^2)])

-- Collatz sequence (take natural num, if even /2, if odd *3 & +1 and repeat)
-- it is thought that every sequence ends with 1
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
  | even x = x : collatz (x `div` 2)
  | otherwise = x : collatz (x*3+1)

-- for all starting numbers between 1 and 100, how many chains have a length >15?
numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
  where isLong xs = length xs > 15

-- map with partially applied functions
multFactors :: (Num a, Enum a) => [a -> a]
multFactors = map (*) [0..]

-- 4) Lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- these two are equivalent
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x y z -> x + y + z

-- 5) Folds
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' e = foldl (\acc x -> acc || x == e) False

map''' :: (a -> a) -> [a] -> [a]
map''' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p = foldl (\acc x -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
numSumRoots :: Int
numSumRoots = length (takeWhile (<=1000) (scanl (\acc x -> acc + sqrt x) 0 [1..]))

-- 6) Function applications with $
numSumRoots' :: Int
numSumRoots' = length $ takeWhile (<=1000) $ scanl (\acc x -> acc + sqrt x) 0 [1..]

mapOverListOfFuncs :: a -> [a -> b] -> [b]
mapOverListOfFuncs k = map ($ k)

-- 7) Function composition
numSumRoots'' :: Int
numSumRoots'' = length . takeWhile (<=1000) $ scanl (\acc x -> acc + sqrt x) 0 [1..]