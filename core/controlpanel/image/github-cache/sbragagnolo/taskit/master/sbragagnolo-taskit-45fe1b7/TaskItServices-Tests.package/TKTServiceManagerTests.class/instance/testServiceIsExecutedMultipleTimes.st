tests
testServiceIsExecutedMultipleTimes

	| executed service |
	executed := 0.
	
	service := TKTParameterizableService new.
	service name: 'test'.
	service step: [ executed := executed + 1 ].
	service start.
	
	10 milliSecond wait.
	service stop.
	200 milliSeconds wait.
	
	self assert: executed > 1