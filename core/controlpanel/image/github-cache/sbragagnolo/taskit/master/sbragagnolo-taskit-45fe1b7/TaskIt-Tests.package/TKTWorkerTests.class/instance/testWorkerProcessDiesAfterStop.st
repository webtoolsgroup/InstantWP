tests
testWorkerProcessDiesAfterStop

	| worker future |
	worker := TKTWorker new.
	worker start.
	worker stop.
	
	future := TKTFuture doing: [ [ worker process isRunning ] whileTrue: [ 50 milliSeconds wait ] ].
	future synchronizeTimeout: 2 seconds.
	
	self deny: worker process isRunning