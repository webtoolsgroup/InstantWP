tests
testWorkerProcessDiesAfterWorkerAndAllFuturesAreCollected

	| worker future process waitFuture1 waitFuture2 |
	worker := TKTWorker new.
	worker start.
	process := worker process.
	
	waitFuture1 := TKTFuture doing: [ [ process isRunning ] whileTrue: [ 50 milliSeconds wait ] ].
	waitFuture2 := TKTFuture doing: [ [ process isRunning ] whileTrue: [ 50 milliSeconds wait ] ].
	
	future := worker future: [ 10 milliSeconds wait ].
	
	
	worker := nil.
	Smalltalk garbageCollect.
	self should: [waitFuture1 synchronizeTimeout: 200 milliSeconds] raise: TKTTimeoutException.
	self assert: process isRunning.
	
	future := nil.
	Smalltalk garbageCollect.
	waitFuture2 synchronizeTimeout: 200 milliSeconds.
	
	self deny: process isRunning.