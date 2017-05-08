tests-callbacks
testCannotDeployFailureTwiceIntoFuture

	| future |
	future := TKTFuture new.
	future deployFailure: Error new.
	self should: [future deployFailure: Error new] raise: Error.