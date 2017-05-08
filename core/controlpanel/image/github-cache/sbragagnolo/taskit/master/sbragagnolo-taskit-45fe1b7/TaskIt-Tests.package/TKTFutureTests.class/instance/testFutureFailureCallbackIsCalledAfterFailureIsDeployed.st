tests-callbacks
testFutureFailureCallbackIsCalledAfterFailureIsDeployed

	| future got deployedError |
	future := TKTFuture new.	
	deployedError := Error new.
	future deployFailure: deployedError.
	
	future onFailureDo: [ :error | got := error ].
	50 milliSeconds wait.
	
	self assert: got equals: deployedError