tests
testWorkerRunsAlwaysInSameProcess

	| worker hash futures hashes |
	worker := TKTWorker new.
	worker start.
	
	futures := (1 to: 10) collect: [ :i | worker future: [ hash := Processor activeProcess identityHash ] ].
	hashes := futures collect: [ :each | each synchronizeTimeout: 1 hour ].
	
	self assert: hashes asSet size equals: 1