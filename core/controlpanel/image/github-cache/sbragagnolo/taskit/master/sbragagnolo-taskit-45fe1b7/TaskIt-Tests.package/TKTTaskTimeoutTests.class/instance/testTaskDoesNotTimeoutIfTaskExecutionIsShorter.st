tests
testTaskDoesNotTimeoutIfTaskExecutionIsShorter

	| runner finished |
	
	finished := false.
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	runner schedule: [ 10 milliSeconds wait. finished := true ] timeout: 50 milliSeconds.
	
	self assert: runner exceptionHandler error equals: nil.
	self assert: finished