tests-combinators
testFutureAndThenSuccessIfAndThenFails

	| future collection thenFuture got |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	thenFuture := future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | self error ];
		andThen: [ :v | collection add: v + 3 ].

	thenFuture onSuccessDo: [ :e | got := e ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2