as yet unclassified
testServiceExecutesTearDownOnStop

	| stopped |
	stopped := false.
	service := TKTParameterizableService new.
	service name: 'test service'.
	service onTearDownDo: [ stopped := true ].
	service start.
	(service stop) waitForCompletion: 1 second.

	self assert: stopped.