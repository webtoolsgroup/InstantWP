tests-callbacks
testFutureSuccessCallbackExecutesInSameLocalProcessRunner

	| runner future currentRunner |
	runner := TKTLocalProcessTaskRunner new.
	future := runner future: [ 1 + 1 ].
	
	future onSuccessDo: [ :r | currentRunner := TKTCurrentTaskRunner value ].
	
	future waitForCompletion: 1 second.
	self assert: currentRunner equals: runner