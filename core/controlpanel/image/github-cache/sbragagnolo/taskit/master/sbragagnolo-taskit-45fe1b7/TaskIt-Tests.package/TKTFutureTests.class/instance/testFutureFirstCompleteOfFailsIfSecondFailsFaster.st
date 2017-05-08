tests-combinators
testFutureFirstCompleteOfFailsIfSecondFailsFaster

	| future got secondFuture error |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	
	(future firstCompleteOf: secondFuture) onFailureDo: [ :v | got := v ].

	error := Error new.
	secondFuture deployFailure: error.
	future deploySuccess: 1.
	50 milliSecond wait.

	self assert: got equals: error