-- Pattern matching

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

-- Guards
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

-- Where



-- Let



-- Case expressions
