as yet unclassified
testCannotStartTwoServicesWithSameName

	| executed service newService |
	executed := 0.
	
	[service := TKTParameterizableService new.
	service name: 'test'.
	service step: [ 100 milliSeconds wait ].
	service start.
	
	newService := TKTParameterizableService new.
	newService name: 'test'.
	newService step: [ 100 milliSeconds wait ].
	self should: [service start] raise: Error ] ensure: [ 
		service stop.
		[newService stop] ifError: [  ].
		200 milliSeconds wait.
	]