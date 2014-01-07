import H99( myLast, myButLast, elementAt, myLength )
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

main = runTestTT $ TestList
			[
				problem1,
				problem2,
				problem3,
				problem4
			]
