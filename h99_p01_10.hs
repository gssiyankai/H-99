module H99( myLast, myButLast ) where

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Problem 2
--myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs
