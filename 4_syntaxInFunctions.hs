-- 1) Pattern matching

sayNum :: (Integral a) => a -> String
sayNum 1 = "One"
sayNum 2 = "Two"
sayNum 3 = "Three"
sayNum n = "Screw this!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

add2dVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
add2dVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

testList = [(1,2), (3,4), (5,6)]
addPairs :: (Num a) => [(a, a)] -> [a]
addPairs xs = [a+b | (a,b) <- xs]

head' :: [a] -> a
head' [] = error "Not possible to call head on empty list" 
-- -> error function causes runtime error, so don't use this too much
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- "as" patterns:
firstLetter :: String -> String
firstLetter "" = "Empty string has no first letter!"
firstLetter letters@(x:_) = "The first letter of " ++ letters ++ " is " ++ [x]

-- 2) Guards
tellBMI :: (RealFloat a) => a -> a -> String
tellBMI weight height
  | weight / height^2 <= 18.5 = "Underweight"
  | weight / height^2 <= 25.0 = "Normal"
  | weight / height^2 <= 30 = "Overweight" 
  | otherwise = "Massive"

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

-- 3) Where
-- Either after guards 
tellBMI' :: (RealFloat a) => a -> a -> String
tellBMI' weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= overweight = "Overweight" 
  | otherwise = "Massive"
  where bmi = weight / height^2
        skinny = 18.5
        normal = 25.0
        overweight = 30.0

-- or after patterns
initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- where can also store functions
calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [bmi w h | (w, h) <- xs]
  where bmi w h = w / h^2

-- 4) Let
sumOfTriple :: (Num a) => (a,a,a) -> a
sumOfTriple triple = let (a,b,c) = triple in a+b+c -- great for dismantling tuples

-- difference to where is that let bindings are expressions not just 
calcBMIs' :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs' xs = [bmi w h | (w,h) <- xs, let bmi w h = w / h^2]  


-- 5) Case expressions
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

