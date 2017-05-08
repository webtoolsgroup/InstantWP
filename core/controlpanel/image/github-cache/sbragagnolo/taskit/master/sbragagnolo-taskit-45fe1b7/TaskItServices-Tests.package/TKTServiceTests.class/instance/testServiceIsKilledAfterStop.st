as yet unclassified
testServiceIsKilledAfterStop

	service := TKTParameterizableService new.
	service name: 'test service'.
	service start.
	(service stop) waitForCompletion: 1 second.
	
	self deny: service isRunning.