tests-callbacks
testFutureSuccessCallbackExecutesInNewProcess

	| runner future processIds processIdsSet |
	processIds := AtomicSharedQueue new.
	
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ 1 + 1 ].
	
	30 timesRepeat: [
		future onSuccessDo: [ :r | processIds nextPut: Processor activeProcess identityHash ] ].
	
	1 second wait.
	
	processIdsSet := Set new.
	[processIds isEmpty] whileFalse: [ processIdsSet add: processIds next ].
	
	self assert: processIdsSet size equals: 30.