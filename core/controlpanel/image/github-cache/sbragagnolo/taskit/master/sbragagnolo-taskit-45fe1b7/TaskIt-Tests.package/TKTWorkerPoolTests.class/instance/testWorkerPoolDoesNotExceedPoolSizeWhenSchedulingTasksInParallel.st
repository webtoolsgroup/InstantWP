tests
testWorkerPoolDoesNotExceedPoolSizeWhenSchedulingTasksInParallel

	| pool totalTasks futures |
	pool := TKTWorkerPool new.
	pool poolMaxSize: 10.
	pool start.
	totalTasks := 0.
		
	futures := AtomicSharedQueue new.
	1 to: 10 do: [:i | 
		futures nextPut: (pool future: [ 
			10 timesRepeat: [ futures nextPut: (pool future: [
					totalTasks := totalTasks + 1 ]) ] ]) ].
	
	[ futures isEmpty ] whileFalse: [ futures next waitForCompletion: 100 milliSeconds ].
	
	self assert: totalTasks equals: 100.
	self assert: pool size equals: 10