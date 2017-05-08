as yet unclassified
tearDown

	super tearDown.
	(service notNil and: [service isRunning]) ifTrue: [
		(service stop) waitForCompletion: 1 second ]