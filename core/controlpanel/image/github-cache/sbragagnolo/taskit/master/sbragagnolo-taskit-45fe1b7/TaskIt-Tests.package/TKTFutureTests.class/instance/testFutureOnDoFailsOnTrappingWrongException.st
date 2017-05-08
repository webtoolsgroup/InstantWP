tests-combinators
testFutureOnDoFailsOnTrappingWrongException

	| future got error |
	future := TKTFuture new.	
	(future on: NotFound do: [ :v | 5 ]) onFailureDo: [ :v | got := v ].

	error := Error new.
	future deployFailure: error.
	50 milliSecond wait.

	self assert: got equals: error