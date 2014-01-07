module H99( myLast, myButLast, elementAt ) where

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x : xs) index 
	| index == 1	= x
	| otherwise	= elementAt xs (index-1)
