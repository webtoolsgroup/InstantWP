tests-combinators
testFutureFlatCollectFailsOnMappedFutureFailure

	| future got error innerFuture |
	future := TKTFuture new.	
	innerFuture := TKTFuture new.
	(future flatCollect: [ :v | innerFuture ]) onFailureDo: [ :v | got := v ].
	
	error := Error new.
	future deploySuccess: 2.
	innerFuture deployFailure: error.
	50 milliSecond wait.
	
	self assert: got equals: error