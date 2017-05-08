tests-combinators
testFutureFirstCompleteOfSuccessIfFirstSuccessFaster

	| future got secondFuture |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	
	(future firstCompleteOf: secondFuture) onSuccessDo: [ :v | got := v ].

	future deploySuccess: 5.
	secondFuture deployFailure: Error new.
	50 milliSecond wait.

	self assert: got equals: 5