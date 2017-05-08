tests
testWorkerPoolDoesNotExceedPoolSize

	| pool |
	pool := TKTWorkerPool new.
	pool poolMaxSize: 4.
	pool start.
		
	((1 to: 10) collect: [:i | pool future: [ 1 + 1 ] ])
		do: [ :future | future waitForCompletion: 1 second. ].
	
	self assert: pool size equals: 4