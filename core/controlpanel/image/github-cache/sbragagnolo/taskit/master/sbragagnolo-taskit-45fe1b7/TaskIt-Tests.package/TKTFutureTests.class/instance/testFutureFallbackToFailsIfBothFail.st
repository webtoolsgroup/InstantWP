tests-combinators
testFutureFallbackToFailsIfBothFail

	| future got fallbackFuture error |
	future := TKTFuture new.
	fallbackFuture := TKTFuture new.	

	(future fallbackTo: fallbackFuture) onFailureDo: [ :v | got := v ].

	error := Error new.
	fallbackFuture deployFailure: error.
	future	 deployFailure: Error new.
	50 milliSecond wait.
	
	self assert: got equals: error