module H99( myLast ) where

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x : xs) = myLast xs
