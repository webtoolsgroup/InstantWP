tests
testLocalProcessTaskRunnerExceptionIsHandledByExceptionHandler

	| runner error |
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	error := Error new.
	runner schedule: [ error signal ].
	
	self assert: runner exceptionHandler error equals: error