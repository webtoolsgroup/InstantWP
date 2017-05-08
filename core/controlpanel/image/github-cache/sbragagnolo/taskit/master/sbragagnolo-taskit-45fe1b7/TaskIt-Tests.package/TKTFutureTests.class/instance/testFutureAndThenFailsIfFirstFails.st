tests-combinators
testFutureAndThenFailsIfFirstFails

	| future collection thenFuture got error |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	thenFuture := future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | collection add: v + 2 ];
		andThen: [ :v | collection add: v + 3 ].

	thenFuture onFailureDo: [ :e | got := e ].
	
	error := Error new.
	future deployFailure: error.
	50 milliSecond wait.
	
	self assert: got equals: error