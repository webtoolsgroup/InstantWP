tests
testWorkerPoolStopStopsWorkers

	| pool |
	pool := TKTWorkerPool new.
	pool poolMaxSize: 4.
	pool start.
		
	((1 to: 10) collect: [:i | pool future: [ 1 + 1 ] ])
		do: [ :future | future waitForCompletion: 500 milliSeconds. ].
	
	pool stop.
	
	pool workers do: [ :worker | self deny: worker isRunning ].