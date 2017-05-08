tests-combinators
testFutureFirstCompleteOfFailsIfFirstFailsFaster

	| future got secondFuture error |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	
	(future firstCompleteOf: secondFuture) onFailureDo: [ :v | got := v ].

	error := Error new.
	future deployFailure: error.
	secondFuture deploySuccess: 1.
	50 milliSecond wait.

	self assert: got equals: error