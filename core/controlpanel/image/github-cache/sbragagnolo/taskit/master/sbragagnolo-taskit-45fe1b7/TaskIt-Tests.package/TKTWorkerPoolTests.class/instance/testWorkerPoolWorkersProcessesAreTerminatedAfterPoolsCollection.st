tests
testWorkerPoolWorkersProcessesAreTerminatedAfterPoolsCollection

	| pool workerProcess |
	pool := TKTWorkerPool new.
	pool name: 'test pool'.
	pool poolMaxSize: 4.
	pool start.
		
	(pool future: [ 1 ])
		waitForCompletion: 1 second.

	workerProcess := pool workers anyOne process process.
	
	pool := nil.
	3 timesRepeat: [Smalltalk garbageCollect. 10 milliSeconds wait ].

	self assert: workerProcess isTerminated