module H99 where

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

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldl (++) [] $ map flatten xs

-- Problem 8
compress :: Eq a => [a] -> [a]
compress xs = foldr (compress') [] xs
		where
			compress' i j
				| null j        = [i]
				| i == (head j) = j
				| otherwise     = i:j

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack list@(x:xs) = let (first,rest) = span (==x) list
			in first : pack rest
pack [] = []

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

-- Problem 11
data ListItem a = Multiple Int a | Single a
	deriving (Show, Eq)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeModified' . encode
	where 
		encodeModified' (1, a) = Single a
		encodeModified' (n, a) = Multiple n a

-- Problem 12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified ((Single x):xs)    = [x] ++ (decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs) 
decodeModified [] = []

-- Problem 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect (x:xs) = encodeDirect' 1 x xs
	where
		encodeDirect' n x (y:xs)
			| x == y	= encodeDirect' (n+1) x xs
			| otherwise	= encodeElement n x : (encodeDirect' 1 y xs)
		encodeDirect' n x [] = [encodeElement n x]
		encodeElement n x
			| n == 1	= Single x
			| otherwise	= Multiple n x
encodeDirect [] = []

-- Problem 14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd $ filter ((n/=) . fst) $ zip (cycle [1..n]) xs

-- Problem 17
split :: [a] -> Int -> ([a],[a])
split (x:xs) n = split' [x] xs n 1
	where split' xs (y:ys) n i
		| n==i		= (xs,(y:ys))
		| otherwise	= split' (xs ++ [y]) ys n (i+1)
	      split' xs [] n i = (xs,[])

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs a b = snd $ split (fst $ split xs b) (a-1)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = drop nn xs ++ take nn xs
	where nn = n `mod` (length xs)

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt elt lst 1 = elt : lst
insertAt elt (x:xs) idx = x : insertAt elt xs (idx-1)

