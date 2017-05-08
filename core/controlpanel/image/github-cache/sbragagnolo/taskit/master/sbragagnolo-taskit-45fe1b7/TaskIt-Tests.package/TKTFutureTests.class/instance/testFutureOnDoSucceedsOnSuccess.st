tests-combinators
testFutureOnDoSucceedsOnSuccess

	| future got |
	future := TKTFuture new.	
	(future on: Error do: [ :v | 5 ]) onSuccessDo: [ :v | got := v ].

	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2