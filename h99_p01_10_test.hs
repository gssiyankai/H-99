import H99( myLast, myButLast )
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

main = runTestTT $ TestList
			[
				problem1,
				problem2
			]
