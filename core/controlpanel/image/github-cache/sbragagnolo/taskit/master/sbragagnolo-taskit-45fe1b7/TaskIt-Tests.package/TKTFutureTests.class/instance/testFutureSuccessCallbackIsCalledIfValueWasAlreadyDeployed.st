tests-callbacks
testFutureSuccessCallbackIsCalledIfValueWasAlreadyDeployed

	| future got |
	future := TKTFuture new.
	future deploySuccess: 2.
		
	future onSuccessDo: [ :value | got := value ].

	50 milliSeconds wait.

	self assert: got equals: 2