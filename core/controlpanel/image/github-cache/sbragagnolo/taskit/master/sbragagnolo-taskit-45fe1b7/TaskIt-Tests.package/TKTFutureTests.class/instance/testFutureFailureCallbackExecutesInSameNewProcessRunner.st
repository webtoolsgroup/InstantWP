tests-callbacks
testFutureFailureCallbackExecutesInSameNewProcessRunner

	| runner future currentRunner |
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ Error signal ].
	
	future onFailureDo: [ :r | currentRunner := TKTCurrentTaskRunner value ].
	future waitForCompletion: 1 second.
	50 milliSecond wait.
	
	self assert: currentRunner equals: runner