as yet unclassified
testStopServiceStopsUnderlyingWorker

	| executed service |
	executed := 0.
	
	service := TKTParameterizableService new.
	service name: 'test'.
	service step: [ executed := executed + 1 ].
	service start.
	service stop.
	200 milliSeconds wait.
	
	self deny: service worker isRunning