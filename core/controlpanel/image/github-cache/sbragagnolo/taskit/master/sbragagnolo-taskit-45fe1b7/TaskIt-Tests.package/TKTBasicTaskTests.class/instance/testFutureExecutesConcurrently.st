tests
testFutureExecutesConcurrently

	| results future1 future2 |
	results := OrderedCollection new.

	future1 := [ 100 milliSeconds wait.
	results addLast: 1 ] future.
	future2 := [ 10 milliSeconds wait.
	results addLast: 2 ] future.
	
	future1 waitForCompletion: 2 seconds.
	future2 waitForCompletion: 2 seconds.
	
	self assert: results asArray equals: #(2 1)	