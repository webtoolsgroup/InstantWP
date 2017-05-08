tests-combinators
testFutureFallbackToHasFirstValueIfFirstSuccess

	| future got fallbackFuture |
	future := TKTFuture new.
	fallbackFuture := TKTFuture new.	

	(future fallbackTo: fallbackFuture) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.

	self assert: got equals: 2