import H99( myLast )
import Test.HUnit

problem1 = TestCase $ do
	assertEqual 
  		"Find the last element of a list of integers" 4 (myLast [1,2,3,4])
	assertEqual 
  		"Find the last element of a list of chars" 'z' (myLast ['x','y','z'])

main = runTestTT $ TestList
			[
				problem1 
			]
