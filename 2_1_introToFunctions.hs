-- An intro to functions
-- ghci> :l intro to load the intro.hs file
-- :r to reload after changes

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2
