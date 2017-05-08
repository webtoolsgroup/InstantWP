tests-callbacks
testFutureSuccessCallbackIsCalledAfterValueIsDeployed

	| future got |
	future := TKTFuture new.
	future onSuccessDo: [ :value | got := value ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2