tests
testServiceExecutesStartUpOnStart

	service := TKTMockService new.
	service start.
	
	10 milliSeconds wait.
	
	self assert: service isStarted.