tests-callbacks
testFutureExecutesInNewProcessRunner

	| runner future currentRunner |
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ currentRunner := TKTCurrentTaskRunner value ].	
	future waitForCompletion: 1 second.
	
	self assert: currentRunner equals: runner