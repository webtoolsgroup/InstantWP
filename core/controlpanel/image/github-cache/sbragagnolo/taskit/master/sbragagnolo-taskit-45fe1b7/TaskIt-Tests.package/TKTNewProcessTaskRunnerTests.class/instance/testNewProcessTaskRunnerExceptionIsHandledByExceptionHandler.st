tests
testNewProcessTaskRunnerExceptionIsHandledByExceptionHandler

	| runner error future |
	runner := TKTNewProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	error := Error new.
	runner schedule: [ error signal ].
	
	"We wait until the error is signaled, trapped and we have it."
	100 milliSeconds wait.
	self assert: runner exceptionHandler error equals: error