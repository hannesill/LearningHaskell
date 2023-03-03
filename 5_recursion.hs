-- 1) Implementing core functions recursively
-- maximum function recursively
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Not possible with empty list!"
maximum' [x] = x 
maximum' (x:xs) = max x (maximum' xs)

-- or if we are not allowed to use max:
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Not possible with empty list!"
maximum'' [x] = x
maximum'' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum'' xs

-- replicate function recursively
replicate' :: (Num i, Ord i) => i -> x -> [x]
replicate' 0 x = []
replicate' n x = x:replicate' (n-1) x

-- take function recursively
take' :: (Num a, Ord a) => a -> [b] -> [b]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

-- or to make it robust against negative numbers
take'' :: (Num a, Ord a) => a -> [b] -> [b]
take'' n _
  | n <= 0 = []
take'' _ [] = []
take'' n (x:xs) = x : take'' (n-1) xs

-- reverse function recursively
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat function recursively ! Attention: infinite list !
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- zip function recursively
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- elem function recursively
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x     = True
  | otherwise  = elem' e xs


-- 2) Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [s | s <- xs, s <= x]
      biggerSorted = quicksort [s | s <- xs, s > x]
  in smallerSorted ++ [x] ++ biggerSorted

