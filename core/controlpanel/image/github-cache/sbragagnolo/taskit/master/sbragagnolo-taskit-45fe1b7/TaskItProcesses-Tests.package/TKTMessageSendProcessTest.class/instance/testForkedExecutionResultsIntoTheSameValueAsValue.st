parallel running test
testForkedExecutionResultsIntoTheSameValueAsValue
	process run.
	20 milliSeconds wait. 
	self assert: (dummyObject at: 'key') equals: 'value'
	

	