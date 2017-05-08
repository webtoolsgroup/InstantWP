tests
testWorkerProcessIsWorkingUntilAllTasksAreDone

	| worker process waitFuture w |
	worker := TKTWorker new.
	worker start.
	process := worker process.
	
	waitFuture := TKTFuture doing: [ [ process isRunning ] whileTrue: [ 50 milliSeconds wait ] ].
	
	worker schedule: [ 100 milliSeconds wait ].
	self assert: process taskQueue size equals: 1.
	50 milliSeconds wait.
	self assert: process taskQueue size equals: 0.
	
	worker := nil.
	Smalltalk garbageCollect.
	
	50 milliSeconds wait.
	Smalltalk garbageCollect.
	self assert: process taskQueue size equals: 1.
	self assert: process isRunning.
	
	[waitFuture waitForCompletion: 1 second] ifError: [].
	
	self deny: process isRunning.