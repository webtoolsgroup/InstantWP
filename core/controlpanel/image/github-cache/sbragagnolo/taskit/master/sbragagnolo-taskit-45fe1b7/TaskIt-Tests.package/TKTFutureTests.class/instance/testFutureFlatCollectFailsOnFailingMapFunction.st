tests-combinators
testFutureFlatCollectFailsOnFailingMapFunction

	| future got error |
	future := TKTFuture new.	
	error := Error new.
	(future flatCollect: [ :v | error signal ]) onFailureDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.

	self assert: got equals: error