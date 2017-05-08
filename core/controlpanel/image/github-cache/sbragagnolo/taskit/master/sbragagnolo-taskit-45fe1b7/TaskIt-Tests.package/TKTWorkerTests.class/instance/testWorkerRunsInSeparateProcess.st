tests
testWorkerRunsInSeparateProcess

	| worker hash future |
	worker := TKTWorker new.
	worker start.
	
	future := worker future: [ hash := Processor activeProcess identityHash ].
	future waitForCompletion: 1 hour.
	
	self assert: hash ~= Processor activeProcess identityHash.