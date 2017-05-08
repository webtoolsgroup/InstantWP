tests-combinators
testFutureZipFailsIfSecondFails

	| future zipFuture got error |
	future := TKTFuture new.
	zipFuture := TKTFuture new.
	
	(future zip: zipFuture) onFailureDo: [ :tuple | got := tuple ].

	error := Error new.
	zipFuture deployFailure: error.
	future deploySuccess: 5.
	50 milliSecond wait.
	
	self assert: got equals: error