tests-termination
testFutureIsFinishedIfFailureWasAlreadyDeployed

	| future |
	future := TKTFuture new.
	future deployFailure: Error new.
		
	self assert: future isFinished