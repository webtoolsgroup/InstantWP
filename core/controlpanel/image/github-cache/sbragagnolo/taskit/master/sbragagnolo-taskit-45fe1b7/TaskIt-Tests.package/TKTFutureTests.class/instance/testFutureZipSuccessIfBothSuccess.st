tests-combinators
testFutureZipSuccessIfBothSuccess

	| future zipFuture got |
	future := TKTFuture new.
	zipFuture := TKTFuture new.
	
	(future zip: zipFuture) onSuccessDo: [ :tuple | got := tuple ].

	future deploySuccess: 2.
	zipFuture deploySuccess: 5.
	50 milliSecond wait.
	
	self assert: got equals: #(2 5)