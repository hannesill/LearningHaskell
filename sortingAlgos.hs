-- Selection sort
-- O(n^2) best, average and worst case time complexity
-- Delete min item from list
-- Sort rest of list (recursion)
-- Append min to beginning of sorted list
delete :: Ord a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) = if x == y then ys
                  else y : delete x ys
                        
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = let m = minimum xs
                       rest = delete m xs
                    in m : selectionSort rest

---------------------------------------------------------------
-- Insertion sort
-- O(n) best case, O(n^2) average & worst case time complexity
-- Sort rest of list
-- Insert first item to sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then y : x:y:ys
                  else y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

---------------------------------------------------------------
-- Bubble sort
-- O(n) best case, O(n^2) average & worst case time complexity
-- ...
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:zs) = if x <= y then x : bubble (y:zs)
                  else y : bubble (x:zs)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = let bubbled = bubble xs
                    mx = last bubbled
                    rest = init bubbled
                in bubbleSort rest ++ [mx]

---------------------------------------------------------------
-- Quick sort
-- O(n log n) best & average case, O(n^2) worst case time complexity
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = let s = [s | s <- xs, s <= x]
                       l = [l | l <- xs, l > x]
                   in quickSort s ++ [x] ++ quickSort l

---------------------------------------------------------------
-- Merge sort
-- O(n log n) best, average, worst case time complexity
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let lhalf = take (length xs `div` 2) xs
                   bhalf = drop (length xs `div` 2) xs
               in merge (mergeSort lhalf) (mergeSort bhalf)

---------------------------------------------------------------
-- Heap sort
-- O(n) best, O(n log n) average & worst case time complexity
