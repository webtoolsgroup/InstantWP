as yet unclassified
testServiceRunsAfterStart

	| run |
	run := false.
	service := TKTParameterizableService new.
	service name: 'test service'.
	service step: [ run := true ].
	service start.
	
	10 milliSeconds wait.
	
	self assert: run.