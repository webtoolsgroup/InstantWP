tests-callbacks
testFutureSuccessCallbackIsNotCalledIfFailureWasAlreadyDeployed

	| future got |
	future := TKTFuture new.
	future deployFailure: Error new.
		
	future onSuccessDo: [ :value | got := value ].

	self assert: got equals: nil