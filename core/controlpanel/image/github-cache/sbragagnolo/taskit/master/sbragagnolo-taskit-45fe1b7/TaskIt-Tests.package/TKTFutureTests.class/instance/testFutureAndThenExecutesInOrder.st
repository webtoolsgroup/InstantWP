tests-combinators
testFutureAndThenExecutesInOrder

	| future collection |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | collection add: v + 2 ];
		andThen: [ :v | collection add: v + 3 ].

	future deploySuccess: 0.
	50 milliSecond wait.
	
	self assert: collection asArray equals: #( 1 2 3)