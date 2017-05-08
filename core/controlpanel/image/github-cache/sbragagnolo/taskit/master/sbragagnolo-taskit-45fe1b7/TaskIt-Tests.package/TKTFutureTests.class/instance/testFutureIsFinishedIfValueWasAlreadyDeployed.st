tests-termination
testFutureIsFinishedIfValueWasAlreadyDeployed

	| future |
	future := TKTFuture new.
	future deploySuccess: 2.
		
	self assert: future isFinished