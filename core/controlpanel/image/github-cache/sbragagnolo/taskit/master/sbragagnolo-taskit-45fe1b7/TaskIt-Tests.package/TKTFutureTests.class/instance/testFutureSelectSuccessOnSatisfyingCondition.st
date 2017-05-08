tests-combinators
testFutureSelectSuccessOnSatisfyingCondition

	| future got |
	future := TKTFuture new.	
	(future select: [ :v | v even ]) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2