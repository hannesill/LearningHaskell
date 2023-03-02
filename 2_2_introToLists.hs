-- Intro to lists

appendLists xs ys = xs ++ ys

appendItemToList x xs = x:xs

getItemFromList x xs = xs !! x

trueValue = [1,2,3] == 1:2:3:[]

headOfList = head [1,2,3,4,5] -- =1
initOfList = init [1,2,3,4,5] -- =[1,2,3,4]
lastOfList = last [1,2,3,4,5] -- =5
tailOfList = tail [1,2,3,4,5] -- =[2,3,4,5]

lengthOfList = length [1,2,3,4,5] -- =5
isEmptyList = null [1,2,3,4,5] -- = False
reverseList = reverse [1,2,3,4,5] -- =[5,4,3,2,1]

takeFromStartOfList = take 3 [1,2,3,4,5] -- =[1,2,3]
takeFromStartOfList' = take 6 [1,2,3,4,5] -- =[1,2,3,4,5]
takeFromStartOfList'' = take 0 [1,2,3,4,5] -- =[]
dropFromStartOfList = drop 3 [1,2,3,4,5] -- =[4,5]
dropFromStartOfList' = drop 6 [1,2,3,4,5] -- =[]
dropFromStartOfList'' = drop 0 [1,2,3,4,5] -- =[1,2,3,4,5]

maximumOfList = maximum [1,2,3,4,5] -- =5
minimumOfList = minimum [1,2,3,4,5] -- =1
sumOfListItems = sum [1,2,3,4,5] -- =15
productOfListItems = product [1,2,3,4,5] -- =120
isElemInList = 4 `elem` [1,2,3,4,5] -- =True

-- Texas ranges
oneToTwenty = [1..20]
aToZ = ['A'..'Z']
infinity = [1..] -- ! will print forever when run !
first24MultiplesOf13 = [13,26..24*13] -- better way:
first24MultiplesOf13' = take 24 [13,26..] -- why better?

-- Functions to produce infinite lists
infiniteCycle = take 10 (cycle [1,2,3])
infiniteReps = take 10 (repeat 1) -- same as cycle [1]. Better
replicateItem = replicate 3 10

-- List comprehension
firstTenEvenNums = [x*2 | x <- [1..10], x*2 >= 12]
-- ...








