module H99 (
	myLast, myButLast, elementAt, myLength, myReverse,
	isPalindrome
	) where

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

-- Problem 4
myLength :: [a] -> Int
myLength list = myLength' list 0
	where 
		myLength' [x] l = l+1
		myLength' (_ : xs) l = myLength' xs l+1

-- Problem 5
myReverse :: [a] -> [a]
myReverse list = myReverse' list []
	where
		myReverse' [] result = result
		myReverse' (x : xs) result = myReverse' xs (x : result)

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
