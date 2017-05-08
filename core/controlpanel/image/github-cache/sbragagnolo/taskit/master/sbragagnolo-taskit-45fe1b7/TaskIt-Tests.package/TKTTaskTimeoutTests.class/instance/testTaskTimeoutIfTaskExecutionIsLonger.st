tests
testTaskTimeoutIfTaskExecutionIsLonger

	| runner |
	runner := TKTLocalProcessTaskRunner new.
	runner exceptionHandler: (TKTTestExceptionHandler new).
	
	runner schedule: [ 50 milliSeconds wait ] timeout: 10 milliSeconds.
	
	self assert: (runner exceptionHandler error isKindOf: TKTCancelledTaskException)