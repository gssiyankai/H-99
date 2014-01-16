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
				problem18
			]
