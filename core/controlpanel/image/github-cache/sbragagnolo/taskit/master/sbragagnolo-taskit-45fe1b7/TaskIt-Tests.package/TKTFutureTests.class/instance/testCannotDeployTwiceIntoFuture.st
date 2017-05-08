tests-callbacks
testCannotDeployTwiceIntoFuture

	| future |
	future := TKTFuture new.
	future deploySuccess: 2.
	self should: [future deploySuccess: 3] raise: Error.