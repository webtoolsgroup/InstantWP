tests-combinators
testFutureFlatCollectSuccessOnMappedFutureSuccess

	| future got innerFuture |
	future := TKTFuture new.	
	innerFuture := TKTFuture new.
	(future flatCollect: [ :v | innerFuture ]) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	innerFuture deploySuccess: 42.
	50 milliSecond wait.
	
	self assert: got equals: 42