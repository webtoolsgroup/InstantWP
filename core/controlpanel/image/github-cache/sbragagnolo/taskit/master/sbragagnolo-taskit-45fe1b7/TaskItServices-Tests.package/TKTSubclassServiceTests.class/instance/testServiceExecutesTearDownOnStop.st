tests
testServiceExecutesTearDownOnStop

	service := TKTMockService new.
	service start.
	(service stop) waitForCompletion: 1 second.

	self assert: service isShutDown.