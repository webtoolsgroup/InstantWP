tests-combinators
testFutureSelectFailsOnFailingPredicate

	| future got error |
	future := TKTFuture new.	
	error := Error new.

	(future select: [ :v | error signal ]) onFailureDo: [ :v | got := v ].

	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: error