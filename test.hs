import H99
import Test.HUnit

problem1 = TestCase $ do
	assertEqual 
  		"Find the last element of a list of integers" 4 (myLast [1,2,3,4])
	assertEqual 
  		"Find the last element of a list of chars" 'z' (myLast ['x','y','z'])

problem2 = TestCase $ do
	assertEqual 
  		"Find the last but one element of a list of integers" 3 (myButLast [1,2,3,4])
	assertEqual 
  		"Find the last but one element of a list of chars" 'y' (myButLast ['a'..'z'])

problem3 = TestCase $ do
	assertEqual 
  		"Find the K'th element of a list of integers" 2 (elementAt [1,2,3] 2)
	assertEqual 
  		"Find the K'th element of a string" 'e' (elementAt "haskell" 5)

problem4 = TestCase $ do
	assertEqual 
  		"Find the number of elements of a list of integers" 3 (myLength [123, 456, 789])
	assertEqual 
  		"Find the length of a string" 13 (myLength "Hello, world!")

problem5 = TestCase $ do
	assertEqual 
  		"Reverse a string" "!amanap ,lanac a ,nalp a ,nam A" (myReverse "A man, a plan, a canal, panama!")
	assertEqual
  		"Reverse a list of integers" [4,3,2,1] (myReverse [1,2,3,4])

problem6 = TestCase $ do
	assertEqual 
  		"Find out whether a list is a palindrome" False (isPalindrome [1,2,3])
	assertEqual 
  		"Find out whether a string is a palindrome" True (isPalindrome "madamimadam")
	assertEqual 
  		"Find out whether a list is a palindrome" True (isPalindrome [1,2,4,8,16,8,4,2,1])

problem7 = TestCase $ do
	assertEqual
		"Flatten a nested list structure" [5] (flatten (Elem 5))
	assertEqual
		"Flatten a nested list structure" [1, 2, 3, 4, 5] (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
--	assertEqual
--		"Flatten a nested list structure" [] (flatten (List []))

problem8 = TestCase $ do
	assertEqual 
  		"Eliminate consecutive duplicates of list elements" "abcade" (compress "aaaabccaadeeee")

problem9 = TestCase $ do
	assertEqual 
  		"Pack consecutive duplicates of list elements into sublists" ["aaaa","b","cc","aa","d","eeee"] (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])

problem10 = TestCase $ do
	assertEqual 
  		"Run-length encoding of a list" [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] (encode "aaaabccaadeeee")

problem11 = TestCase $ do
	assertEqual 
  		"Modified run-length encoding" [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] (encodeModified "aaaabccaadeeee")

problem12 = TestCase $ do
	assertEqual 
  		"Decode a run-length encoded list" "aaaabccaadeeee" (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'])

problem13 = TestCase $ do
	assertEqual 
  		"Run-length encoding of a list (direct solution)" [Multiple 4 'a',Single 'b',Multiple 2 'c',  Multiple 2 'a',Single 'd',Multiple 4 'e'] (encodeDirect "aaaabccaadeeee")

problem14 = TestCase $ do
	assertEqual 
  		"Duplicate the elements of a list" [1,1,2,2,3,3] (dupli [1, 2, 3])

problem15 = TestCase $ do
	assertEqual 
  		"Replicate the elements of a list a given number of times" "aaabbbccc" (repli "abc" 3)

problem16 = TestCase $ do
	assertEqual 
  		"Drop every N'th element from a list" "abdeghk" (dropEvery "abcdefghik" 3)

problem17 = TestCase $ do
	assertEqual 
  		"Split a list into two parts; the length of the first part is given" ("abc", "defghik") (split "abcdefghik" 3)
	assertEqual 
  		"Split a list into two parts; the length of the first part is given" ("abcdefghik", []) (split "abcdefghik" 15)

problem18 = TestCase $ do
	assertEqual 
  		"Extract a slice from a list" "cdefg" (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7)

problem19 = TestCase $ do
	assertEqual 
  		"Rotate a list N places to the left" "defghabc" (rotate ['a','b','c','d','e','f','g','h'] 3)
	assertEqual 
  		"Rotate a list N places to the left" "ghabcdef" (rotate ['a','b','c','d','e','f','g','h'] (-2))

problem20 = TestCase $ do
	assertEqual 
  		"Remove the K'th element from a list" ('b',"acd") (removeAt 2 "abcd")

problem21 = TestCase $ do
	assertEqual 
  		"Insert an element at a given position into a list" "aXbcd" (insertAt 'X' "abcd" 2)

problem22 = TestCase $ do
	assertEqual 
  		"Create a list containing all integers within a given range" [4,5,6,7,8,9] (range 4 9)

problem31 = TestCase $ do
	assertEqual 
  		"Determine whether a given integer number is prime" True (isPrime 7)

problem32 = TestCase $ do
	assertEqual 
  		"Determine the greatest common divisor of two positive integer numbers" [9,3,3] [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]

problem33 = TestCase $ do
	assertEqual 
  		"Determine whether two positive integer numbers are coprime" True (coprime 35 64)

problem35 = TestCase $ do
	assertEqual 
  		"Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order" [3,3,5,7] (primeFactors 315)

problem36 = TestCase $ do
	assertEqual 
  		"Determine the prime factors of a given positive integer" [(3,2),(5,1),(7,1)] (prime_factors_mult 315)

problem39 = TestCase $ do
	assertEqual 
  		"A list of prime numbers" [11,13,17,19] (primesR 10 20)

problem40 = TestCase $ do
	assertEqual 
  		"Goldbach's conjecture" (5, 23) (goldbach 28)


main = runTestTT $ TestList
			[
				problem1,
				problem2,
				problem3,
				problem4,
				problem5,
				problem6,
				problem7,
				problem8,
				problem9,
				problem10,
				problem11,
				problem12,
				problem13,
				problem14,
				problem15,
				problem16,
				problem17,
				problem18,
				problem19,
				problem20,
				problem21,
				problem22,
				problem31,
				problem32,
				problem33,
				problem35,
				problem36,
				problem39,
				problem40
			]
