import qualified Data.Map as Map

-- 1) Algebraic data types intro

-- Define new data type
data Bool' = False | True

-- Shape is a type. Circle and Rectangle are not! They are value constructors
-- Shape is part of Show (with deriving keyword)
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- With intermediate data type
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- function that takes shape and returns surface
surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- function that moves a shape by x and y value
move :: Shape -> Float -> Float -> Shape
move (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
move (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- auxilliary functions for if we don't want to deal with points
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- 2) Record syntax & 4) Derived instances
data Car = Car { company :: String -- record syntax
               , model :: String
               , year :: Int
               } deriving (Eq, Show) -- derived instances

newCar1 = Car "Porsche" "Taycan" 2022
newCar2 = Car {company="Ferrari", model="Roma", year=2021}
newCar3 = Car {model="DBS 770", company="Aston Martin", year=2023}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- 3) Type parameters & 5) Type synonyms
type String' = [Char]
type AssocList k v = [(k,v)] -- parameterized type synonym
type IntMap v = Map.Map Int v -- partially applied parameters
type IntMap' = Map.Map Int -- equal to the type synonym above

-- Example for Either type
data LockerState = Free | Taken deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num mapLockers =
  case Map.lookup num mapLockers of
    Nothing -> Left "The locker doesn't exist!"
    Just (state,code) -> if state == Free 
                            then Right code
                            else Left $ "Locker " ++ show num ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

-- 6) Recursive data structures
