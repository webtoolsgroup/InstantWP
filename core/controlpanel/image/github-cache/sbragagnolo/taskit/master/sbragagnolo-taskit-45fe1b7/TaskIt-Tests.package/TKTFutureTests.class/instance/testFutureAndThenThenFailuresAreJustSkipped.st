tests-combinators
testFutureAndThenThenFailuresAreJustSkipped

	| future collection thenFuture |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	thenFuture := future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | self error ];
		andThen: [ :v | collection add: v + 3 ].

	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: collection asArray equals: #(3 5)